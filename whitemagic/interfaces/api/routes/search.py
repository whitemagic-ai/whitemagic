"""Semantic search API routes."""


from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from ...search import SearchMode, SemanticSearcher  # type: ignore[import-not-found]
from ..dependencies import CurrentUser
from ..memory_service import get_memory_manager  # type: ignore[import-not-found]

router = APIRouter(prefix="/search", tags=["Search"])


# Request/Response Models
class SemanticSearchRequest(BaseModel):
    """Request for semantic search."""

    query: str = Field(..., description="Search query")
    mode: SearchMode = Field(SearchMode.HYBRID, description="Search mode")
    k: int = Field(10, ge=1, le=100, description="Number of results")
    threshold: float = Field(0.7, ge=0.0, le=1.0, description="Similarity threshold")
    memory_type: str | None = Field(None, description="Filter by type")
    tags: list[str] | None = Field(None, description="Filter by tags")
    keyword_weight: float = Field(0.3, ge=0.0, le=1.0)
    semantic_weight: float = Field(0.7, ge=0.0, le=1.0)


class SemanticSearchResultItem(BaseModel):
    """Single search result."""

    memory_id: str
    title: str
    content: str
    type: str
    tags: list[str]
    score: float
    match_type: str


class SemanticSearchResponse(BaseModel):
    """Search response."""

    success: bool = True
    query: str
    mode: str
    results: list[SemanticSearchResultItem]
    count: int
    execution_time_ms: float


@router.post("/semantic", response_model=SemanticSearchResponse)
async def semantic_search(
    request: SemanticSearchRequest, user: CurrentUser,
) -> SemanticSearchResponse:
    """Semantic search endpoint."""
    import time

    start = time.time()

    try:
        manager = get_memory_manager(user)
        searcher = SemanticSearcher(manager)

        results = await searcher.search(
            query=request.query,
            mode=request.mode,
            k=request.k,
            threshold=request.threshold,
            keyword_weight=request.keyword_weight,
            semantic_weight=request.semantic_weight,
            memory_type=request.memory_type,
            tags=request.tags,
        )

        result_items = [
            SemanticSearchResultItem(
                memory_id=r.memory_id,
                title=r.title,
                content=r.content,
                type=r.type,
                tags=r.tags,
                score=r.score,
                match_type=r.match_type,
            )
            for r in results
        ]

        execution_time = (time.time() - start) * 1000

        return SemanticSearchResponse(
            query=request.query,
            mode=request.mode.value,
            results=result_items,
            count=len(result_items),
            execution_time_ms=round(execution_time, 2),
        )

    except ValueError as e:
        raise HTTPException(400, str(e))
    except Exception as e:
        raise HTTPException(500, f"Search failed: {str(e)}")
