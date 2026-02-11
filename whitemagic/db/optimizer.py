# mypy: disable-error-code=no-untyped-def
"""Database Query Optimization for WhiteMagic

Provides:
- Query optimization utilities
- Index management
- Query analysis
- Performance monitoring
- Connection pooling optimization
"""

import json
import time
from collections.abc import Callable, Generator
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime
from functools import wraps
from typing import Any, TypeVar, cast

try:
    from sqlalchemy import Index, inspect, text
    from sqlalchemy.engine import Engine
    from sqlalchemy.exc import SQLAlchemyError
    from sqlalchemy.orm import Session
    SQLALCHEMY_AVAILABLE = True
except ImportError:
    SQLALCHEMY_AVAILABLE = False

from whitemagic.cache import get_redis_cache
from whitemagic.logging_config import get_logger

logger = get_logger(__name__)

T = TypeVar("T")


@dataclass
class QueryStats:
    """Query performance statistics."""

    query: str
    execution_time: float
    rows_returned: int
    timestamp: datetime
    parameters: dict[str, Any] | None = None
    cached: bool = False


@dataclass
class IndexRecommendation:
    """Index recommendation for optimization."""

    table: str
    columns: list[str]
    index_type: str  # btree, hash, gist, gin
    estimated_impact: str  # high, medium, low
    reason: str


class QueryOptimizer:
    """Database query optimization manager."""

    def __init__(self, engine: Engine, cache_ttl: int = 300):
        self.engine = engine
        self.cache_ttl = cache_ttl
        self.query_stats: list[QueryStats] = []
        self.slow_query_threshold = 1.0  # seconds
        self._setup_monitoring()

    def _setup_monitoring(self) -> None:
        """Set up query monitoring."""
        # Enable query logging if in debug mode
        if logger.level <= 10:  # DEBUG level
            self.engine.echo = True

    @contextmanager
    def monitor_query(self, query: str, parameters: dict[str, Any] | None = None) -> Generator[None, None, None]:
        """Context manager to monitor query performance."""
        start_time = time.time()
        cached = False

        try:
            yield
        finally:
            execution_time = time.time() - start_time

            # Log slow queries
            if execution_time > self.slow_query_threshold:
                logger.warning(
                    f"Slow query detected: {execution_time:.3f}s - {query[:100]}...",
                )

            # Store stats
            stat = QueryStats(
                query=query,
                execution_time=execution_time,
                rows_returned=0,  # Would need to be set by caller
                timestamp=datetime.now(),
                parameters=parameters,
                cached=cached,
            )

            self.query_stats.append(stat)

            # Keep only last 1000 stats
            if len(self.query_stats) > 1000:
                self.query_stats = self.query_stats[-1000:]

    def analyze_query(self, query: str) -> dict[str, Any]:
        """Analyze a query for optimization opportunities."""
        if not SQLALCHEMY_AVAILABLE:
            return {"error": "SQLAlchemy not available"}

        try:
            with self.engine.connect() as conn:
                # Get query execution plan
                explain_query = f"EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) {query}"
                result = conn.execute(text(explain_query))
                plan = result.scalar()

                # Extract key metrics
                analysis: dict[str, Any] = {
                    "execution_plan": plan[0] if plan else {},
                    "estimated_cost": plan[0]["Plan"]["Total Cost"] if plan else None,
                    "estimated_rows": plan[0]["Plan"]["Plan Rows"] if plan else None,
                    "recommendations": [],
                }

                # Generate recommendations
                analysis["recommendations"] = self._generate_recommendations(plan[0] if plan else {})

                return analysis

        except SQLAlchemyError as e:
            logger.error(f"Query analysis failed: {e}")
            return {"error": str(e)}

    def _generate_recommendations(self, plan: dict[str, Any]) -> list[str]:
        """Generate optimization recommendations from execution plan."""
        recommendations = []

        # Check for sequential scans
        if "Seq Scan" in str(plan):
            recommendations.append(
                "Consider adding an index to avoid sequential scan",
            )

        # Check for high cost
        if plan.get("Total Cost", 0) > 1000:
            recommendations.append(
                "Query has high cost - consider breaking into smaller queries",
            )

        # Check for missing indexes
        if "Filter" in str(plan) and "Index Scan" not in str(plan):
            recommendations.append(
                "Consider adding an index on filtered columns",
            )

        return recommendations

    def get_slow_queries(self, limit: int = 10) -> list[QueryStats]:
        """Get list of slow queries."""
        return sorted(
            self.query_stats,
            key=lambda x: x.execution_time,
            reverse=True,
        )[:limit]

    def get_query_performance_report(self) -> dict[str, Any]:
        """Generate query performance report."""
        if not self.query_stats:
            return {"message": "No query statistics available"}

        # Calculate metrics
        total_queries = len(self.query_stats)
        avg_time = sum(s.execution_time for s in self.query_stats) / total_queries
        slow_queries = [s for s in self.query_stats if s.execution_time > self.slow_query_threshold]

        # Group by query type
        query_types = {}
        for stat in self.query_stats:
            query_type = stat.query.split()[0].upper()
            if query_type not in query_types:
                query_types[query_type] = {"count": 0, "total_time": 0.0}
            query_types[query_type]["count"] += 1
            query_types[query_type]["total_time"] += stat.execution_time

        return {
            "total_queries": total_queries,
            "average_time": avg_time,
            "slow_queries": {
                "count": len(slow_queries),
                "percentage": (len(slow_queries) / total_queries) * 100,
            },
            "query_types": query_types,
            "top_slow": self.get_slow_queries(5),
        }


class IndexManager:
    """Database index management."""

    def __init__(self, engine: Engine):
        self.engine = engine
        self.inspector = inspect(engine)

    def get_table_indexes(self, table_name: str) -> list[dict[str, Any]]:
        """Get all indexes for a table."""
        return list(self.inspector.get_indexes(table_name))

    def analyze_missing_indexes(self, table_name: str, sample_queries: list[str]) -> list[IndexRecommendation]:
        """Analyze queries to recommend missing indexes."""
        recommendations = []

        # Analyze WHERE clauses in queries
        columns_in_where = set()

        for query in sample_queries:
            # Simple parsing for WHERE clauses
            if "WHERE" in query.upper():
                where_part = query.upper().split("WHERE")[1].split("ORDER BY")[0].split("GROUP BY")[0]
                # Extract column names (simplified)
                words = where_part.split()
                for i, word in enumerate(words):
                    if word in ["AND", "OR"] and i + 1 < len(words):
                        col = words[i + 1].strip("=").strip("(").strip(")")
                        if col.isalnum():
                            columns_in_where.add(col.lower())

        # Check if these columns are indexed
        existing_indexes = self.get_table_indexes(table_name)
        indexed_columns: set[str] = set()
        for idx in existing_indexes:
            indexed_columns.update(col.lower() for col in idx["column_names"])

        # Recommend indexes for non-indexed columns
        for col in columns_in_where:
            if col not in indexed_columns:
                recommendations.append(IndexRecommendation(
                    table=table_name,
                    columns=[col],
                    index_type="btree",
                    estimated_impact="medium",
                    reason=f"Column '{col}' frequently used in WHERE clause",
                ))

        return recommendations

    def create_index(
        self,
        table_name: str,
        columns: list[str],
        index_type: str = "btree",
        unique: bool = False,
        name: str | None = None,
    ) -> bool:
        """Create an index on a table."""
        if not SQLALCHEMY_AVAILABLE:
            return False

        try:
            # Generate index name if not provided
            if not name:
                name = f"idx_{table_name}_{'_'.join(columns)}"

            # Create index
            index = Index(
                name,
                *[text(col) for col in columns],
                unique=unique,
                postgresql_where=None,  # Can add partial index condition here
                postgresql_using=index_type,
            )

            # Add to table metadata
            self.engine.get_table_metadata(table_name)
            index.create(self.engine)

            logger.info(f"Created index {name} on {table_name}({', '.join(columns)})")
            return True

        except SQLAlchemyError as e:
            logger.error(f"Failed to create index: {e}")
            return False

    def drop_index(self, index_name: str, table_name: str) -> bool:
        """Drop an index."""
        try:
            with self.engine.connect() as conn:
                conn.execute(text(f"DROP INDEX IF EXISTS {index_name}"))
                conn.commit()

            logger.info(f"Dropped index {index_name}")
            return True

        except SQLAlchemyError as e:
            logger.error(f"Failed to drop index: {e}")
            return False


class QueryCache:
    """Query result caching integration."""

    def __init__(self, redis_cache: Any = None, default_ttl: int = 300) -> None:
        self.cache = redis_cache or get_redis_cache()
        self.default_ttl = default_ttl

    def get_cache_key(self, query: str, params: dict[str, Any] | None = None) -> str:
        """Generate cache key for query."""
        import hashlib
        key_data = f"query:{query}"
        if params:
            key_data += f":{json.dumps(params, sort_keys=True)}"
        return hashlib.md5(key_data.encode()).hexdigest()

    def get_cached_result(self, query: str, params: dict[str, Any] | None = None) -> list[dict] | None:
        """Get cached query result."""
        key = self.get_cache_key(query, params)
        result = self.cache.get(key)
        return list(result) if result is not None else None

    def cache_result(
        self,
        query: str,
        result: list[dict],
        params: dict[str, Any] | None = None,
        ttl: int | None = None,
    ) -> bool:
        """Cache query result."""
        key = self.get_cache_key(query, params)
        return bool(self.cache.set(key, result, ttl or self.default_ttl))

    def invalidate_table_cache(self, table_name: str) -> int:
        """Invalidate cache for a specific table."""
        pattern = f"query:*{table_name}*"
        return int(self.cache.clear(pattern))


# Decorators for query optimization
def optimize_query(cache_ttl: int | None = None, analyze: bool = False) -> Callable:
    """Decorator to optimize and cache query results."""
    def decorator(func: Callable[..., T]) -> Callable[..., T]:
        @wraps(func)
        def wrapper(*args: Any, **kwargs: Any) -> T:
            # Get query from function if possible
            query = getattr(func, "__query__", None)

            if query and cache_ttl:
                # Try cache first
                cache = get_redis_cache()
                cached = cache.get(f"query_func:{func.__name__}")
                if cached is not None:
                    return cast(T, cached)

            # Execute function
            result = func(*args, **kwargs)

            # Cache result
            if query and cache_ttl:
                cache = get_redis_cache()
                cache.set(f"query_func:{func.__name__}", result, cache_ttl)

            # Analyze if requested
            if analyze and query:
                optimizer = QueryOptimizer(getattr(func, "__engine__", None))
                analysis = optimizer.analyze_query(query)
                logger.info(f"Query analysis for {func.__name__}: {analysis}")

            return result

        return wrapper
    return decorator


def bulk_insert(session: Session, model_class, data: list[dict], batch_size: int = 1000) -> int:
    """Perform bulk insert with optimization."""
    if not SQLALCHEMY_AVAILABLE:
        return 0

    try:
        total_inserted = 0

        for i in range(0, len(data), batch_size):
            batch = data[i:i + batch_size]
            session.bulk_insert_mappings(model_class, batch)
            session.commit()
            total_inserted += len(batch)

        return total_inserted

    except SQLAlchemyError as e:
        session.rollback()
        logger.error(f"Bulk insert failed: {e}")
        return 0


# Example usage
if __name__ == "__main__":
    if SQLALCHEMY_AVAILABLE:
        from sqlalchemy import create_engine

        # Example setup
        engine = create_engine("postgresql://user:pass@localhost/whitemagic")
        optimizer = QueryOptimizer(engine)
        index_manager = IndexManager(engine)

        # Analyze a query
        analysis = optimizer.analyze_query("SELECT * FROM memories WHERE created_at > '2024-01-01'")
        logger.info(f"Query analysis: {analysis}")

        # Get performance report
        report = optimizer.get_query_performance_report()
        logger.info(f"Performance report: {report}")
    else:
        logger.info("SQLAlchemy not available")
