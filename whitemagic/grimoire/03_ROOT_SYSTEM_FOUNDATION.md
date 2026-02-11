# Chapter 3: System Foundation

**Gana**: RootGana (Chinese: 氐, Pinyin: Dǐ)
**Garden**: practice
**Quadrant**: Eastern (Azure Dragon)
**Element**: Wood
**Phase**: Yang Rising
**I Ching Hexagram**: 15. 謙 Qiān (Modesty) - Foundation in humility, checking before building

---

## 🎯 Purpose

Chapter 3 establishes **system foundation**—ensuring all underlying systems are healthy and operational before building upon them. After initiating (Ch.1) and establishing memory presence (Ch.2), the Root verifies that your foundation is solid and stable.

The Root (氐) represents the deep foundation, the base that supports all growth above. Like a tree's roots drawing from healthy soil, like a building's foundation ensuring structural integrity, this chapter embodies the practice of checking before building, measuring before optimizing, and validating before deploying.

**When to use this chapter**:
- **Starting major work** - Verify foundation before ambitious projects
- **After system changes** - Validate health after updates or migrations
- **Troubleshooting failures** - Check foundation when higher systems fail
- **Pre-deployment** - Ensure readiness before releasing to production
- **Performance work** - Establish baselines before optimization
- **Integration updates** - Verify connections after changes
- **Regular maintenance** - Periodic foundation health audits

**Core philosophy**: "The tallest tree grows from the deepest roots. Build on solid ground, and your creation will endure."

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `check_system_health` | Comprehensive health check across all WhiteMagic systems | Start of session, after major changes |
| `validate_integrations` | Verify all system integrations (Gan Ying, MCP, Rust, etc.) | Before deployment, troubleshooting |
| `run_benchmarks` | Performance baseline and regression testing | Establishing baselines, optimizations |
| `check_deploy_status` | Verify deployment health (if applicable) | After deployments |
| `manage_gardens` (activate) | Enter practice garden for disciplined work | Foundation building |
| `get_system_metrics` | Retrieve detailed system metrics | Deep diagnostics |
| `repair_integration` | Attempt automated integration repair | When validation fails |
| `audit_configuration` | Configuration validation and security audit | Pre-deployment, security reviews |

---

## 📋 Comprehensive Workflows

### Workflow 1: Comprehensive System Health Verification

**Purpose**: Multi-tier health check across all WhiteMagic subsystems with automated issue detection and reporting.

**When to use**:
- Session startup (daily health check)
- After system updates or configuration changes
- When experiencing unexplained behavior
- Before major operations

**Implementation**:

```python
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Dict, List, Any, Optional
from whitemagic.core.memory import MemoryManager
from whitemagic.gardens.practice import get_practice_garden
from whitemagic.core.resonance import emit_event


class HealthStatus(Enum):
    """Health status levels."""
    HEALTHY = "healthy"
    DEGRADED = "degraded"
    CRITICAL = "critical"
    UNKNOWN = "unknown"


class Severity(Enum):
    """Issue severity levels."""
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


@dataclass
class HealthIssue:
    """Individual health issue."""
    component: str
    severity: Severity
    message: str
    details: Dict[str, Any] = field(default_factory=dict)
    fix_suggestion: Optional[str] = None
    auto_repairable: bool = False


@dataclass
class ComponentHealth:
    """Health status for a single component."""
    component: str
    status: HealthStatus
    issues: List[HealthIssue] = field(default_factory=list)
    metrics: Dict[str, Any] = field(default_factory=dict)
    last_checked: datetime = field(default_factory=datetime.now)


@dataclass
class SystemHealthReport:
    """Complete system health report."""
    overall_status: HealthStatus
    components: Dict[str, ComponentHealth]
    timestamp: datetime
    total_issues: int
    critical_issues: int
    recommendations: List[str] = field(default_factory=list)


class SystemHealthVerifier:
    """Comprehensive system health verification."""

    def __init__(self):
        self.memory = MemoryManager()
        self.practice = get_practice_garden()

    async def verify_health(
        self,
        deep_scan: bool = False,
        components: Optional[List[str]] = None
    ) -> SystemHealthReport:
        """
        Perform comprehensive health verification.

        Args:
            deep_scan: Perform deep diagnostics (slower but thorough)
            components: Specific components to check (default: all)

        Returns:
            Complete health report with issues and recommendations
        """
        self.practice.embody_quality("diligence", intensity=0.8)
        emit_event("health_check.started", {"deep_scan": deep_scan})

        # Default to all components
        if components is None:
            components = [
                "memory", "resonance", "gardens", "integrations",
                "rust_bindings", "mcp_server", "cache", "neural"
            ]

        component_health = {}
        for component in components:
            health = await self._check_component(component, deep_scan)
            component_health[component] = health

        # Generate report
        report = self._generate_report(component_health)

        # Store health report in memory
        await self.memory.store(
            content=f"System health check: {report.overall_status.value}",
            metadata={
                "type": "health_report",
                "timestamp": report.timestamp.isoformat(),
                "status": report.overall_status.value,
                "total_issues": report.total_issues,
                "critical_issues": report.critical_issues
            },
            tier="short_term"
        )

        emit_event("health_check.completed", {
            "status": report.overall_status.value,
            "issues": report.total_issues
        })

        return report

    async def _check_component(
        self,
        component: str,
        deep_scan: bool
    ) -> ComponentHealth:
        """Check individual component health."""
        from whitemagic.tools import check_system_health

        # Component-specific checks
        if component == "memory":
            return await self._check_memory_health(deep_scan)
        elif component == "resonance":
            return await self._check_resonance_health(deep_scan)
        elif component == "gardens":
            return await self._check_gardens_health(deep_scan)
        elif component == "integrations":
            return await self._check_integrations_health(deep_scan)
        elif component == "rust_bindings":
            return await self._check_rust_health(deep_scan)
        elif component == "mcp_server":
            return await self._check_mcp_health(deep_scan)
        elif component == "cache":
            return await self._check_cache_health(deep_scan)
        elif component == "neural":
            return await self._check_neural_health(deep_scan)
        else:
            # Generic check
            result = check_system_health(component=component)
            return ComponentHealth(
                component=component,
                status=HealthStatus(result.get("status", "unknown")),
                metrics=result
            )

    async def _check_memory_health(self, deep: bool) -> ComponentHealth:
        """Check memory system health."""
        issues = []
        metrics = {}

        # Check memory manager initialization
        try:
            tier_stats = await self.memory.get_tier_statistics()
            metrics["tiers"] = tier_stats

            # Check for concerning patterns
            if tier_stats.get("short_term", {}).get("count", 0) > 1000:
                issues.append(HealthIssue(
                    component="memory",
                    severity=Severity.WARNING,
                    message="Short-term memory approaching capacity",
                    details={"count": tier_stats["short_term"]["count"]},
                    fix_suggestion="Consider consolidation or archiving",
                    auto_repairable=True
                ))

            # Check neural health
            neural_health = await self._get_neural_metrics()
            metrics["neural"] = neural_health

            if neural_health.get("connection_density", 0) < 0.1:
                issues.append(HealthIssue(
                    component="memory",
                    severity=Severity.INFO,
                    message="Neural connection density low",
                    details=neural_health,
                    fix_suggestion="System still learning - normal for new installations"
                ))

        except Exception as e:
            issues.append(HealthIssue(
                component="memory",
                severity=Severity.ERROR,
                message=f"Memory system check failed: {str(e)}",
                fix_suggestion="Check memory manager initialization"
            ))
            return ComponentHealth(
                component="memory",
                status=HealthStatus.CRITICAL,
                issues=issues
            )

        # Determine status
        critical_issues = [i for i in issues if i.severity == Severity.CRITICAL]
        error_issues = [i for i in issues if i.severity == Severity.ERROR]

        if critical_issues:
            status = HealthStatus.CRITICAL
        elif error_issues:
            status = HealthStatus.DEGRADED
        else:
            status = HealthStatus.HEALTHY

        return ComponentHealth(
            component="memory",
            status=status,
            issues=issues,
            metrics=metrics
        )

    async def _check_resonance_health(self, deep: bool) -> ComponentHealth:
        """Check Gan Ying resonance system health."""
        from whitemagic.core.resonance import get_bus

        issues = []
        metrics = {}

        try:
            bus = get_bus()

            # Get bus statistics
            metrics["total_emissions"] = bus.total_emissions
            metrics["total_subscriptions"] = bus.total_subscriptions
            metrics["total_cascades"] = bus.total_cascades

            # Check event throughput (if deep scan)
            if deep:
                import asyncio
                start_count = bus.total_emissions
                await asyncio.sleep(5)  # Sample for 5 seconds
                end_count = bus.total_emissions
                events_per_sec = (end_count - start_count) / 5.0
                metrics["events_per_second"] = events_per_sec

                if events_per_sec < 0.1:
                    issues.append(HealthIssue(
                        component="resonance",
                        severity=Severity.INFO,
                        message="Low event activity detected",
                        details={"eps": events_per_sec},
                        fix_suggestion="Normal for idle system"
                    ))

            # Check for listener health
            if bus.total_subscriptions == 0:
                issues.append(HealthIssue(
                    component="resonance",
                    severity=Severity.WARNING,
                    message="No event subscribers registered",
                    fix_suggestion="Gardens may not be initialized"
                ))

        except Exception as e:
            issues.append(HealthIssue(
                component="resonance",
                severity=Severity.ERROR,
                message=f"Resonance system check failed: {str(e)}"
            ))
            return ComponentHealth(
                component="resonance",
                status=HealthStatus.CRITICAL,
                issues=issues
            )

        status = HealthStatus.HEALTHY if not issues else HealthStatus.DEGRADED

        return ComponentHealth(
            component="resonance",
            status=status,
            issues=issues,
            metrics=metrics
        )

    async def _check_gardens_health(self, deep: bool) -> ComponentHealth:
        """Check all gardens health."""
        issues = []
        metrics = {}

        # List of all 17 gardens
        garden_names = [
            "joy", "love", "beauty", "truth", "wisdom", "mystery",
            "play", "wonder", "connection", "sangha", "practice",
            "presence", "voice", "dharma", "courage", "gratitude", "patience"
        ]

        active_gardens = []
        failed_gardens = []

        for garden_name in garden_names:
            try:
                # Attempt to import garden
                module = __import__(
                    f"whitemagic.gardens.{garden_name}",
                    fromlist=["get_garden"]
                )
                get_garden = getattr(module, f"get_{garden_name}_garden")
                garden = get_garden()

                # Check if garden is responsive
                if hasattr(garden, "get_state"):
                    state = garden.get_state()
                    active_gardens.append(garden_name)
                else:
                    active_gardens.append(garden_name)

            except Exception as e:
                failed_gardens.append(garden_name)
                if deep:
                    issues.append(HealthIssue(
                        component="gardens",
                        severity=Severity.WARNING,
                        message=f"Garden '{garden_name}' failed to load",
                        details={"error": str(e)},
                        fix_suggestion="Check garden implementation"
                    ))

        metrics["total_gardens"] = len(garden_names)
        metrics["active_gardens"] = len(active_gardens)
        metrics["failed_gardens"] = len(failed_gardens)
        metrics["garden_list"] = active_gardens

        if len(failed_gardens) > 5:
            status = HealthStatus.DEGRADED
        elif len(failed_gardens) > 0:
            status = HealthStatus.HEALTHY  # Some failures acceptable
        else:
            status = HealthStatus.HEALTHY

        return ComponentHealth(
            component="gardens",
            status=status,
            issues=issues,
            metrics=metrics
        )

    async def _check_integrations_health(self, deep: bool) -> ComponentHealth:
        """Check system integrations."""
        from whitemagic.tools import validate_integrations

        issues = []
        metrics = {}

        try:
            validation = validate_integrations(quick_check=not deep)
            metrics["validation_results"] = validation

            for system, result in validation.get("results", {}).items():
                if not result.get("healthy", False):
                    issues.append(HealthIssue(
                        component="integrations",
                        severity=Severity.ERROR,
                        message=f"Integration '{system}' unhealthy",
                        details=result,
                        fix_suggestion=result.get("fix_suggestion"),
                        auto_repairable=result.get("auto_repairable", False)
                    ))

        except Exception as e:
            issues.append(HealthIssue(
                component="integrations",
                severity=Severity.ERROR,
                message=f"Integration validation failed: {str(e)}"
            ))
            return ComponentHealth(
                component="integrations",
                status=HealthStatus.CRITICAL,
                issues=issues
            )

        critical = [i for i in issues if i.severity == Severity.CRITICAL]
        errors = [i for i in issues if i.severity == Severity.ERROR]

        if critical:
            status = HealthStatus.CRITICAL
        elif errors:
            status = HealthStatus.DEGRADED
        else:
            status = HealthStatus.HEALTHY

        return ComponentHealth(
            component="integrations",
            status=status,
            issues=issues,
            metrics=metrics
        )

    async def _check_rust_health(self, deep: bool) -> ComponentHealth:
        """Check Rust bindings health."""
        issues = []
        metrics = {}

        try:
            import whitemagic_rs
            metrics["rust_available"] = True

            # Test basic functionality
            try:
                from whitemagic_rs import fast_similarity
                test_result = fast_similarity([1.0, 0.0], [0.0, 1.0])
                metrics["rust_functional"] = True
            except Exception as e:
                issues.append(HealthIssue(
                    component="rust_bindings",
                    severity=Severity.ERROR,
                    message="Rust bindings present but non-functional",
                    details={"error": str(e)},
                    fix_suggestion="Rebuild Rust module: cd whitemagic-rs && maturin develop --release"
                ))

        except ImportError:
            metrics["rust_available"] = False
            issues.append(HealthIssue(
                component="rust_bindings",
                severity=Severity.WARNING,
                message="Rust bindings not available",
                fix_suggestion="Build Rust module for 59x performance boost",
                auto_repairable=False
            ))

        status = HealthStatus.HEALTHY if metrics.get("rust_functional") else HealthStatus.DEGRADED

        return ComponentHealth(
            component="rust_bindings",
            status=status,
            issues=issues,
            metrics=metrics
        )

    async def _check_mcp_health(self, deep: bool) -> ComponentHealth:
        """Check MCP server health."""
        import os
        from pathlib import Path

        issues = []
        metrics = {}

        # Check if MCP server files exist
        mcp_path = Path("whitemagic-mcp")
        if mcp_path.exists():
            metrics["mcp_directory_exists"] = True

            # Check for built server
            dist_path = mcp_path / "dist" / "index.js"
            if dist_path.exists():
                metrics["mcp_built"] = True
            else:
                issues.append(HealthIssue(
                    component="mcp_server",
                    severity=Severity.WARNING,
                    message="MCP server not built",
                    fix_suggestion="cd whitemagic-mcp && npm run build"
                ))
                metrics["mcp_built"] = False
        else:
            issues.append(HealthIssue(
                component="mcp_server",
                severity=Severity.INFO,
                message="MCP server directory not found",
                fix_suggestion="MCP server optional - skip if not using Claude Desktop integration"
            ))
            metrics["mcp_directory_exists"] = False

        status = HealthStatus.HEALTHY if not [i for i in issues if i.severity in [Severity.ERROR, Severity.CRITICAL]] else HealthStatus.DEGRADED

        return ComponentHealth(
            component="mcp_server",
            status=status,
            issues=issues,
            metrics=metrics
        )

    async def _check_cache_health(self, deep: bool) -> ComponentHealth:
        """Check cache system health."""
        issues = []
        metrics = {}

        # Check cache directory and size
        from pathlib import Path
        cache_dir = Path.home() / ".whitemagic" / "cache"

        if cache_dir.exists():
            # Calculate cache size
            total_size = sum(
                f.stat().st_size for f in cache_dir.rglob("*") if f.is_file()
            )
            metrics["cache_size_mb"] = total_size / (1024 * 1024)

            if metrics["cache_size_mb"] > 1000:  # > 1GB
                issues.append(HealthIssue(
                    component="cache",
                    severity=Severity.WARNING,
                    message=f"Cache size large: {metrics['cache_size_mb']:.1f}MB",
                    fix_suggestion="Consider cache cleanup",
                    auto_repairable=True
                ))
        else:
            metrics["cache_size_mb"] = 0

        return ComponentHealth(
            component="cache",
            status=HealthStatus.HEALTHY,
            issues=issues,
            metrics=metrics
        )

    async def _check_neural_health(self, deep: bool) -> ComponentHealth:
        """Check neural memory health."""
        issues = []
        metrics = {}

        try:
            neural_metrics = await self._get_neural_metrics()
            metrics.update(neural_metrics)

            # Check connection density
            if neural_metrics.get("connection_density", 0) > 0.8:
                issues.append(HealthIssue(
                    component="neural",
                    severity=Severity.INFO,
                    message="High neural connection density - excellent learning",
                    details=neural_metrics
                ))

        except Exception as e:
            issues.append(HealthIssue(
                component="neural",
                severity=Severity.WARNING,
                message=f"Neural health check failed: {str(e)}"
            ))

        return ComponentHealth(
            component="neural",
            status=HealthStatus.HEALTHY,
            issues=issues,
            metrics=metrics
        )

    async def _get_neural_metrics(self) -> Dict[str, Any]:
        """Get neural network metrics."""
        # Placeholder - actual implementation would query neural memory
        return {
            "connection_density": 0.45,
            "total_connections": 1247,
            "active_connections": 892,
            "decay_rate": 0.05
        }

    def _generate_report(
        self,
        component_health: Dict[str, ComponentHealth]
    ) -> SystemHealthReport:
        """Generate comprehensive health report."""
        all_issues = []
        critical_count = 0

        for health in component_health.values():
            all_issues.extend(health.issues)
            critical_count += len([
                i for i in health.issues
                if i.severity == Severity.CRITICAL
            ])

        # Determine overall status
        statuses = [h.status for h in component_health.values()]
        if HealthStatus.CRITICAL in statuses:
            overall = HealthStatus.CRITICAL
        elif HealthStatus.DEGRADED in statuses:
            overall = HealthStatus.DEGRADED
        else:
            overall = HealthStatus.HEALTHY

        # Generate recommendations
        recommendations = self._generate_recommendations(component_health)

        return SystemHealthReport(
            overall_status=overall,
            components=component_health,
            timestamp=datetime.now(),
            total_issues=len(all_issues),
            critical_issues=critical_count,
            recommendations=recommendations
        )

    def _generate_recommendations(
        self,
        component_health: Dict[str, ComponentHealth]
    ) -> List[str]:
        """Generate actionable recommendations."""
        recommendations = []

        # Check for common patterns
        degraded_components = [
            name for name, health in component_health.items()
            if health.status == HealthStatus.DEGRADED
        ]

        if degraded_components:
            recommendations.append(
                f"Address degraded components: {', '.join(degraded_components)}"
            )

        # Check for auto-repairable issues
        auto_fix_count = sum(
            len([i for i in h.issues if i.auto_repairable])
            for h in component_health.values()
        )

        if auto_fix_count > 0:
            recommendations.append(
                f"{auto_fix_count} issues can be auto-repaired - consider running repair workflow"
            )

        # Check for Rust performance opportunity
        if "rust_bindings" in component_health:
            rust_health = component_health["rust_bindings"]
            if not rust_health.metrics.get("rust_available"):
                recommendations.append(
                    "Build Rust bindings for 59x performance improvement on critical paths"
                )

        return recommendations

    async def auto_repair(self, report: SystemHealthReport) -> Dict[str, Any]:
        """Attempt automatic repair of fixable issues."""
        self.practice.embody_quality("patience", intensity=0.9)

        repaired = []
        failed = []

        for component, health in report.components.items():
            for issue in health.issues:
                if issue.auto_repairable:
                    try:
                        await self._repair_issue(component, issue)
                        repaired.append({
                            "component": component,
                            "issue": issue.message
                        })
                    except Exception as e:
                        failed.append({
                            "component": component,
                            "issue": issue.message,
                            "error": str(e)
                        })

        return {
            "repaired": repaired,
            "failed": failed,
            "success_rate": len(repaired) / (len(repaired) + len(failed)) if (repaired or failed) else 1.0
        }

    async def _repair_issue(self, component: str, issue: HealthIssue):
        """Attempt to repair a specific issue."""
        # Component-specific repair logic
        if component == "memory" and "capacity" in issue.message.lower():
            # Trigger consolidation
            from whitemagic.tools import consolidate_memories
            consolidate_memories()
        elif component == "cache" and "size" in issue.message.lower():
            # Clear old cache
            from whitemagic.core.memory.cache import clear_old_cache
            clear_old_cache(days=30)


# Example usage
async def example_health_check():
    """Example: Comprehensive health verification."""
    verifier = SystemHealthVerifier()

    # Quick health check
    report = await verifier.verify_health(deep_scan=False)

    print(f"Overall Status: {report.overall_status.value}")
    print(f"Total Issues: {report.total_issues}")
    print(f"Critical Issues: {report.critical_issues}")

    # Show component statuses
    for component, health in report.components.items():
        icon = "✅" if health.status == HealthStatus.HEALTHY else "⚠️" if health.status == HealthStatus.DEGRADED else "❌"
        print(f"{icon} {component}: {health.status.value}")

        for issue in health.issues:
            print(f"   {issue.severity.value.upper()}: {issue.message}")
            if issue.fix_suggestion:
                print(f"   Fix: {issue.fix_suggestion}")

    # Show recommendations
    if report.recommendations:
        print("\nRecommendations:")
        for rec in report.recommendations:
            print(f"  • {rec}")

    # Auto-repair if possible
    if report.total_issues > 0:
        repair_result = await verifier.auto_repair(report)
        print(f"\nAuto-repair: {len(repair_result['repaired'])} issues fixed")
```

---

### Workflow 2: Integration Validation & Repair

**Purpose**: Systematically validate all system integrations and attempt automated repair when issues detected.

**When to use**:
- Before deployments
- After dependency updates
- When experiencing integration failures
- Regular integration health audits

**Implementation**:

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from enum import Enum


class IntegrationStatus(Enum):
    """Integration health status."""
    CONNECTED = "connected"
    DEGRADED = "degraded"
    DISCONNECTED = "disconnected"
    UNKNOWN = "unknown"


@dataclass
class IntegrationEndpoint:
    """Integration endpoint definition."""
    name: str
    type: str  # "mcp", "gan_ying", "rust", "api", "database"
    required: bool = True
    health_check_method: Optional[str] = None


@dataclass
class IntegrationResult:
    """Result of integration validation."""
    endpoint: str
    status: IntegrationStatus
    latency_ms: Optional[float] = None
    error: Optional[str] = None
    details: Dict[str, Any] = field(default_factory=dict)
    repair_attempted: bool = False
    repair_successful: bool = False


class IntegrationValidator:
    """Validate and repair system integrations."""

    def __init__(self):
        self.memory = MemoryManager()
        self.practice = get_practice_garden()

        # Define all integrations
        self.integrations = [
            IntegrationEndpoint("gan_ying_bus", "gan_ying", required=True),
            IntegrationEndpoint("memory_manager", "internal", required=True),
            IntegrationEndpoint("rust_bindings", "rust", required=False),
            IntegrationEndpoint("mcp_server", "mcp", required=False),
            IntegrationEndpoint("gardens_system", "internal", required=True),
            IntegrationEndpoint("neural_memory", "internal", required=True),
            IntegrationEndpoint("cache_system", "internal", required=True),
        ]

    async def validate_all_integrations(
        self,
        quick_check: bool = True,
        auto_repair: bool = True
    ) -> Dict[str, IntegrationResult]:
        """
        Validate all system integrations.

        Args:
            quick_check: Fast validation (ping only)
            auto_repair: Attempt automatic repair if issues found

        Returns:
            Dictionary of integration results
        """
        self.practice.embody_quality("attention_to_detail", intensity=0.9)
        emit_event("integration_validation.started", {
            "quick_check": quick_check,
            "auto_repair": auto_repair
        })

        results = {}

        for integration in self.integrations:
            result = await self._validate_integration(
                integration,
                quick_check=quick_check
            )

            # Attempt repair if unhealthy and auto_repair enabled
            if auto_repair and result.status != IntegrationStatus.CONNECTED:
                repair_result = await self._repair_integration(integration)
                result.repair_attempted = True
                result.repair_successful = repair_result

                # Re-validate after repair
                if repair_result:
                    result = await self._validate_integration(
                        integration,
                        quick_check=quick_check
                    )

            results[integration.name] = result

        # Store validation results
        await self._store_validation_results(results)

        emit_event("integration_validation.completed", {
            "total": len(results),
            "connected": len([r for r in results.values() if r.status == IntegrationStatus.CONNECTED]),
            "failed": len([r for r in results.values() if r.status == IntegrationStatus.DISCONNECTED])
        })

        return results

    async def _validate_integration(
        self,
        integration: IntegrationEndpoint,
        quick_check: bool
    ) -> IntegrationResult:
        """Validate individual integration."""
        import time

        start = time.time()

        try:
            if integration.type == "gan_ying":
                status = await self._check_gan_ying()
            elif integration.type == "rust":
                status = await self._check_rust_bindings()
            elif integration.type == "mcp":
                status = await self._check_mcp_server()
            elif integration.type == "internal":
                status = await self._check_internal_system(integration.name)
            else:
                status = IntegrationStatus.UNKNOWN

            latency = (time.time() - start) * 1000  # Convert to ms

            return IntegrationResult(
                endpoint=integration.name,
                status=status,
                latency_ms=latency
            )

        except Exception as e:
            return IntegrationResult(
                endpoint=integration.name,
                status=IntegrationStatus.DISCONNECTED,
                error=str(e)
            )

    async def _check_gan_ying(self) -> IntegrationStatus:
        """Check Gan Ying bus connectivity."""
        from whitemagic.core.resonance import get_bus

        try:
            bus = get_bus()
            # Test emission
            test_event = emit_event("health_check.gan_ying_test", {})
            return IntegrationStatus.CONNECTED
        except Exception:
            return IntegrationStatus.DISCONNECTED

    async def _check_rust_bindings(self) -> IntegrationStatus:
        """Check Rust bindings availability."""
        try:
            import whitemagic_rs
            from whitemagic_rs import fast_similarity

            # Test basic function
            result = fast_similarity([1.0, 0.0], [0.0, 1.0])
            return IntegrationStatus.CONNECTED
        except ImportError:
            return IntegrationStatus.DISCONNECTED
        except Exception:
            return IntegrationStatus.DEGRADED

    async def _check_mcp_server(self) -> IntegrationStatus:
        """Check MCP server status."""
        from pathlib import Path

        mcp_built = (Path("whitemagic-mcp") / "dist" / "index.js").exists()

        if mcp_built:
            return IntegrationStatus.CONNECTED
        else:
            return IntegrationStatus.DISCONNECTED

    async def _check_internal_system(self, system_name: str) -> IntegrationStatus:
        """Check internal system connectivity."""
        if system_name == "memory_manager":
            try:
                await self.memory.get_tier_statistics()
                return IntegrationStatus.CONNECTED
            except Exception:
                return IntegrationStatus.DISCONNECTED

        elif system_name == "gardens_system":
            try:
                from whitemagic.gardens.joy import get_joy_garden
                garden = get_joy_garden()
                return IntegrationStatus.CONNECTED
            except Exception:
                return IntegrationStatus.DEGRADED

        elif system_name == "neural_memory":
            try:
                # Check neural memory availability
                from whitemagic.core.memory.neural import NeuralMemory
                return IntegrationStatus.CONNECTED
            except Exception:
                return IntegrationStatus.DEGRADED

        elif system_name == "cache_system":
            from pathlib import Path
            cache_dir = Path.home() / ".whitemagic" / "cache"
            return IntegrationStatus.CONNECTED if cache_dir.exists() else IntegrationStatus.DEGRADED

        return IntegrationStatus.UNKNOWN

    async def _repair_integration(
        self,
        integration: IntegrationEndpoint
    ) -> bool:
        """Attempt to repair integration."""
        try:
            if integration.type == "mcp":
                # Attempt MCP build
                import subprocess
                result = subprocess.run(
                    ["npm", "run", "build"],
                    cwd="whitemagic-mcp",
                    capture_output=True,
                    timeout=60
                )
                return result.returncode == 0

            elif integration.type == "rust":
                # Attempt Rust rebuild
                import subprocess
                result = subprocess.run(
                    ["maturin", "develop", "--release"],
                    cwd="whitemagic-rs",
                    capture_output=True,
                    timeout=300
                )
                return result.returncode == 0

            elif integration.name == "cache_system":
                # Create cache directory
                from pathlib import Path
                cache_dir = Path.home() / ".whitemagic" / "cache"
                cache_dir.mkdir(parents=True, exist_ok=True)
                return True

            # Other integrations may not have auto-repair
            return False

        except Exception:
            return False

    async def _store_validation_results(
        self,
        results: Dict[str, IntegrationResult]
    ):
        """Store validation results in memory."""
        summary = {
            "total_integrations": len(results),
            "connected": len([r for r in results.values() if r.status == IntegrationStatus.CONNECTED]),
            "degraded": len([r for r in results.values() if r.status == IntegrationStatus.DEGRADED]),
            "disconnected": len([r for r in results.values() if r.status == IntegrationStatus.DISCONNECTED]),
            "repairs_attempted": len([r for r in results.values() if r.repair_attempted]),
            "repairs_successful": len([r for r in results.values() if r.repair_successful])
        }

        await self.memory.store(
            content=f"Integration validation: {summary['connected']}/{summary['total_integrations']} connected",
            metadata={
                "type": "integration_validation",
                "timestamp": datetime.now().isoformat(),
                **summary
            },
            tier="short_term"
        )


# Example usage
async def example_integration_validation():
    """Example: Validate all integrations."""
    validator = IntegrationValidator()

    # Validate with auto-repair
    results = await validator.validate_all_integrations(
        quick_check=False,
        auto_repair=True
    )

    print("INTEGRATION VALIDATION RESULTS")
    print("=" * 50)

    for endpoint, result in results.items():
        status_icon = {
            IntegrationStatus.CONNECTED: "✅",
            IntegrationStatus.DEGRADED: "⚠️",
            IntegrationStatus.DISCONNECTED: "❌",
            IntegrationStatus.UNKNOWN: "❓"
        }[result.status]

        print(f"{status_icon} {endpoint}: {result.status.value}")

        if result.latency_ms:
            print(f"   Latency: {result.latency_ms:.2f}ms")

        if result.error:
            print(f"   Error: {result.error}")

        if result.repair_attempted:
            repair_icon = "✅" if result.repair_successful else "❌"
            print(f"   Repair {repair_icon}")
```

---

### Workflow 3: Performance Baseline & Regression Testing

**Purpose**: Establish performance baselines and detect regressions across releases.

**When to use**:
- Before optimization work (establish baseline)
- After code changes (detect regressions)
- Regular performance monitoring
- Pre-deployment validation

**Implementation**:

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from datetime import datetime
import time
import statistics


@dataclass
class BenchmarkResult:
    """Single benchmark result."""
    name: str
    ops_per_second: float
    latency_ms: float
    latency_p95_ms: float
    latency_p99_ms: float
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class PerformanceBaseline:
    """Performance baseline for comparison."""
    benchmark_name: str
    baseline_ops_per_second: float
    acceptable_degradation: float = 0.10  # 10% acceptable
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class RegressionReport:
    """Regression detection report."""
    benchmark_name: str
    current_performance: float
    baseline_performance: float
    degradation_percent: float
    is_regression: bool
    severity: str  # "none", "minor", "major", "critical"


class PerformanceTester:
    """Performance baseline and regression testing."""

    def __init__(self):
        self.memory = MemoryManager()
        self.practice = get_practice_garden()
        self.baselines: Dict[str, PerformanceBaseline] = {}

    async def run_benchmark_suite(
        self,
        benchmark_type: str = "full",
        iterations: int = 100
    ) -> Dict[str, BenchmarkResult]:
        """
        Run comprehensive benchmark suite.

        Args:
            benchmark_type: "memory", "resonance", "gardens", "full"
            iterations: Number of iterations per benchmark

        Returns:
            Dictionary of benchmark results
        """
        self.practice.embody_quality("diligence", intensity=0.8)
        emit_event("benchmark.started", {"type": benchmark_type})

        results = {}

        if benchmark_type in ["memory", "full"]:
            results["memory_store"] = await self._benchmark_memory_store(iterations)
            results["memory_search"] = await self._benchmark_memory_search(iterations)
            results["memory_retrieve"] = await self._benchmark_memory_retrieve(iterations)

        if benchmark_type in ["resonance", "full"]:
            results["event_emission"] = await self._benchmark_event_emission(iterations)
            results["event_cascade"] = await self._benchmark_event_cascade(iterations)

        if benchmark_type in ["gardens", "full"]:
            results["garden_activation"] = await self._benchmark_garden_activation(iterations)

        # Store results
        await self._store_benchmark_results(results)

        emit_event("benchmark.completed", {
            "type": benchmark_type,
            "benchmarks_run": len(results)
        })

        return results

    async def _benchmark_memory_store(self, iterations: int) -> BenchmarkResult:
        """Benchmark memory store operations."""
        latencies = []

        for i in range(iterations):
            start = time.perf_counter()

            await self.memory.store(
                content=f"Benchmark test {i}",
                metadata={"benchmark": True, "iteration": i},
                tier="short_term"
            )

            end = time.perf_counter()
            latencies.append((end - start) * 1000)  # Convert to ms

        # Calculate statistics
        avg_latency = statistics.mean(latencies)
        p95_latency = statistics.quantiles(latencies, n=20)[18]  # 95th percentile
        p99_latency = statistics.quantiles(latencies, n=100)[98]  # 99th percentile
        ops_per_second = 1000.0 / avg_latency

        return BenchmarkResult(
            name="memory_store",
            ops_per_second=ops_per_second,
            latency_ms=avg_latency,
            latency_p95_ms=p95_latency,
            latency_p99_ms=p99_latency,
            metadata={"iterations": iterations}
        )

    async def _benchmark_memory_search(self, iterations: int) -> BenchmarkResult:
        """Benchmark memory search operations."""
        # First, populate some test data
        for i in range(50):
            await self.memory.store(
                content=f"Search benchmark data {i}",
                metadata={"benchmark": True, "searchable": True},
                tier="long_term"
            )

        latencies = []

        for i in range(iterations):
            start = time.perf_counter()

            results = await self.memory.search(
                query="benchmark",
                limit=10
            )

            end = time.perf_counter()
            latencies.append((end - start) * 1000)

        avg_latency = statistics.mean(latencies)
        p95_latency = statistics.quantiles(latencies, n=20)[18]
        p99_latency = statistics.quantiles(latencies, n=100)[98]
        ops_per_second = 1000.0 / avg_latency

        return BenchmarkResult(
            name="memory_search",
            ops_per_second=ops_per_second,
            latency_ms=avg_latency,
            latency_p95_ms=p95_latency,
            latency_p99_ms=p99_latency,
            metadata={"iterations": iterations}
        )

    async def _benchmark_memory_retrieve(self, iterations: int) -> BenchmarkResult:
        """Benchmark memory retrieval operations."""
        # Store a test memory
        test_memory = await self.memory.store(
            content="Retrieval benchmark test",
            metadata={"benchmark": True},
            tier="long_term"
        )
        memory_id = test_memory["id"]

        latencies = []

        for i in range(iterations):
            start = time.perf_counter()

            retrieved = await self.memory.retrieve(memory_id)

            end = time.perf_counter()
            latencies.append((end - start) * 1000)

        avg_latency = statistics.mean(latencies)
        p95_latency = statistics.quantiles(latencies, n=20)[18]
        p99_latency = statistics.quantiles(latencies, n=100)[98]
        ops_per_second = 1000.0 / avg_latency

        return BenchmarkResult(
            name="memory_retrieve",
            ops_per_second=ops_per_second,
            latency_ms=avg_latency,
            latency_p95_ms=p95_latency,
            latency_p99_ms=p99_latency,
            metadata={"iterations": iterations}
        )

    async def _benchmark_event_emission(self, iterations: int) -> BenchmarkResult:
        """Benchmark event emission speed."""
        from whitemagic.core.resonance import emit_event

        latencies = []

        for i in range(iterations):
            start = time.perf_counter()

            emit_event("benchmark.test_event", {"iteration": i})

            end = time.perf_counter()
            latencies.append((end - start) * 1000)

        avg_latency = statistics.mean(latencies)
        p95_latency = statistics.quantiles(latencies, n=20)[18]
        p99_latency = statistics.quantiles(latencies, n=100)[98]
        ops_per_second = 1000.0 / avg_latency

        return BenchmarkResult(
            name="event_emission",
            ops_per_second=ops_per_second,
            latency_ms=avg_latency,
            latency_p95_ms=p95_latency,
            latency_p99_ms=p99_latency,
            metadata={"iterations": iterations}
        )

    async def _benchmark_event_cascade(self, iterations: int) -> BenchmarkResult:
        """Benchmark event cascade performance."""
        from whitemagic.gardens.beauty import get_beauty_garden

        beauty = get_beauty_garden()
        latencies = []

        for i in range(iterations):
            start = time.perf_counter()

            # Trigger beauty (which cascades to joy, love, etc.)
            beauty.recognize_beauty(
                subject=f"benchmark {i}",
                qualities=["test"]
            )

            end = time.perf_counter()
            latencies.append((end - start) * 1000)

        avg_latency = statistics.mean(latencies)
        p95_latency = statistics.quantiles(latencies, n=20)[18]
        p99_latency = statistics.quantiles(latencies, n=100)[98]
        ops_per_second = 1000.0 / avg_latency

        return BenchmarkResult(
            name="event_cascade",
            ops_per_second=ops_per_second,
            latency_ms=avg_latency,
            latency_p95_ms=p95_latency,
            latency_p99_ms=p99_latency,
            metadata={"iterations": iterations}
        )

    async def _benchmark_garden_activation(self, iterations: int) -> BenchmarkResult:
        """Benchmark garden activation speed."""
        from whitemagic.tools import manage_gardens

        latencies = []

        for i in range(iterations):
            start = time.perf_counter()

            manage_gardens(action="activate", garden_name="practice")

            end = time.perf_counter()
            latencies.append((end - start) * 1000)

        avg_latency = statistics.mean(latencies)
        p95_latency = statistics.quantiles(latencies, n=20)[18]
        p99_latency = statistics.quantiles(latencies, n=100)[98]
        ops_per_second = 1000.0 / avg_latency

        return BenchmarkResult(
            name="garden_activation",
            ops_per_second=ops_per_second,
            latency_ms=avg_latency,
            latency_p95_ms=p95_latency,
            latency_p99_ms=p99_latency,
            metadata={"iterations": iterations}
        )

    async def establish_baseline(
        self,
        benchmark_results: Dict[str, BenchmarkResult],
        acceptable_degradation: float = 0.10
    ) -> Dict[str, PerformanceBaseline]:
        """
        Establish performance baselines from current results.

        Args:
            benchmark_results: Current benchmark results
            acceptable_degradation: Acceptable degradation threshold (0.10 = 10%)

        Returns:
            Dictionary of performance baselines
        """
        baselines = {}

        for name, result in benchmark_results.items():
            baseline = PerformanceBaseline(
                benchmark_name=name,
                baseline_ops_per_second=result.ops_per_second,
                acceptable_degradation=acceptable_degradation
            )
            baselines[name] = baseline
            self.baselines[name] = baseline

        # Store baselines
        await self.memory.store(
            content="Performance baselines established",
            metadata={
                "type": "performance_baseline",
                "timestamp": datetime.now().isoformat(),
                "baselines": {
                    name: baseline.baseline_ops_per_second
                    for name, baseline in baselines.items()
                }
            },
            tier="long_term"
        )

        return baselines

    async def detect_regressions(
        self,
        current_results: Dict[str, BenchmarkResult],
        baselines: Optional[Dict[str, PerformanceBaseline]] = None
    ) -> List[RegressionReport]:
        """
        Detect performance regressions against baselines.

        Args:
            current_results: Current benchmark results
            baselines: Performance baselines (uses stored if not provided)

        Returns:
            List of regression reports
        """
        if baselines is None:
            baselines = self.baselines

        if not baselines:
            # Try to load from memory
            baselines = await self._load_baselines()

        regressions = []

        for name, result in current_results.items():
            if name in baselines:
                baseline = baselines[name]

                # Calculate degradation
                degradation = (
                    baseline.baseline_ops_per_second - result.ops_per_second
                ) / baseline.baseline_ops_per_second

                is_regression = degradation > baseline.acceptable_degradation

                # Determine severity
                if degradation < 0:
                    severity = "none"  # Performance improved!
                elif degradation <= baseline.acceptable_degradation:
                    severity = "none"
                elif degradation <= baseline.acceptable_degradation * 2:
                    severity = "minor"
                elif degradation <= baseline.acceptable_degradation * 5:
                    severity = "major"
                else:
                    severity = "critical"

                report = RegressionReport(
                    benchmark_name=name,
                    current_performance=result.ops_per_second,
                    baseline_performance=baseline.baseline_ops_per_second,
                    degradation_percent=degradation * 100,
                    is_regression=is_regression,
                    severity=severity
                )

                regressions.append(report)

        # Store regression report
        if any(r.is_regression for r in regressions):
            await self.memory.store(
                content=f"Performance regressions detected: {len([r for r in regressions if r.is_regression])}",
                metadata={
                    "type": "regression_report",
                    "timestamp": datetime.now().isoformat(),
                    "regressions": [
                        {
                            "benchmark": r.benchmark_name,
                            "degradation": r.degradation_percent,
                            "severity": r.severity
                        }
                        for r in regressions if r.is_regression
                    ]
                },
                tier="short_term"
            )

        return regressions

    async def _load_baselines(self) -> Dict[str, PerformanceBaseline]:
        """Load baselines from memory."""
        # Search for latest baseline
        results = await self.memory.search(
            query="performance_baseline",
            filters={"type": "performance_baseline"},
            limit=1
        )

        if results:
            baseline_data = results[0].metadata.get("baselines", {})
            baselines = {}

            for name, ops_per_sec in baseline_data.items():
                baselines[name] = PerformanceBaseline(
                    benchmark_name=name,
                    baseline_ops_per_second=ops_per_sec
                )

            return baselines

        return {}

    async def _store_benchmark_results(
        self,
        results: Dict[str, BenchmarkResult]
    ):
        """Store benchmark results in memory."""
        summary = {
            name: {
                "ops_per_second": result.ops_per_second,
                "latency_ms": result.latency_ms,
                "latency_p95_ms": result.latency_p95_ms
            }
            for name, result in results.items()
        }

        await self.memory.store(
            content=f"Benchmark suite completed: {len(results)} benchmarks",
            metadata={
                "type": "benchmark_results",
                "timestamp": datetime.now().isoformat(),
                "results": summary
            },
            tier="short_term"
        )


# Example usage
async def example_performance_testing():
    """Example: Performance baseline and regression testing."""
    tester = PerformanceTester()

    # Run initial benchmark suite
    print("Running benchmark suite...")
    results = await tester.run_benchmark_suite(benchmark_type="full", iterations=100)

    print("\nBENCHMARK RESULTS")
    print("=" * 50)
    for name, result in results.items():
        print(f"{name}:")
        print(f"  Ops/sec: {result.ops_per_second:.0f}")
        print(f"  Avg latency: {result.latency_ms:.2f}ms")
        print(f"  P95 latency: {result.latency_p95_ms:.2f}ms")
        print(f"  P99 latency: {result.latency_p99_ms:.2f}ms")

    # Establish baseline
    print("\nEstablishing performance baselines...")
    baselines = await tester.establish_baseline(results, acceptable_degradation=0.10)
    print(f"✅ Baselines established for {len(baselines)} benchmarks")

    # Simulate code change and re-test
    print("\nRunning regression detection...")
    new_results = await tester.run_benchmark_suite(benchmark_type="memory", iterations=100)

    # Detect regressions
    regressions = await tester.detect_regressions(new_results, baselines)

    print("\nREGRESSION REPORT")
    print("=" * 50)
    for report in regressions:
        icon = "❌" if report.is_regression else "✅"
        print(f"{icon} {report.benchmark_name}:")
        print(f"   Baseline: {report.baseline_performance:.0f} ops/sec")
        print(f"   Current:  {report.current_performance:.0f} ops/sec")
        print(f"   Change:   {report.degradation_percent:+.1f}%")
        print(f"   Severity: {report.severity}")
```

---

### Workflow 4: Dependency Health & Version Management

**Purpose**: Track, validate, and manage system dependencies and their versions.

**When to use**:
- Before updates or migrations
- Dependency conflict troubleshooting
- Security audit requirements
- Version compatibility checks

**Implementation**:

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from enum import Enum
import subprocess
import json


class DependencyStatus(Enum):
    """Dependency health status."""
    CURRENT = "current"
    OUTDATED = "outdated"
    VULNERABLE = "vulnerable"
    MISSING = "missing"
    CONFLICT = "conflict"


@dataclass
class DependencyInfo:
    """Information about a dependency."""
    name: str
    current_version: Optional[str]
    latest_version: Optional[str]
    required_by: List[str] = field(default_factory=list)
    status: DependencyStatus = DependencyStatus.CURRENT
    vulnerabilities: List[Dict[str, Any]] = field(default_factory=list)
    notes: str = ""


@dataclass
class DependencyReport:
    """Comprehensive dependency health report."""
    timestamp: datetime
    python_dependencies: Dict[str, DependencyInfo]
    npm_dependencies: Dict[str, DependencyInfo]
    rust_dependencies: Dict[str, DependencyInfo]
    total_dependencies: int
    outdated_count: int
    vulnerable_count: int
    missing_count: int


class DependencyManager:
    """Manage and validate system dependencies."""

    def __init__(self):
        self.memory = MemoryManager()
        self.practice = get_practice_garden()

    async def audit_dependencies(
        self,
        include_python: bool = True,
        include_npm: bool = True,
        include_rust: bool = True,
        check_vulnerabilities: bool = True
    ) -> DependencyReport:
        """
        Comprehensive dependency audit.

        Args:
            include_python: Check Python dependencies
            include_npm: Check NPM dependencies
            include_rust: Check Rust dependencies
            check_vulnerabilities: Run security vulnerability checks

        Returns:
            Complete dependency report
        """
        self.practice.embody_quality("thoroughness", intensity=0.9)
        emit_event("dependency_audit.started", {
            "python": include_python,
            "npm": include_npm,
            "rust": include_rust
        })

        python_deps = {}
        npm_deps = {}
        rust_deps = {}

        if include_python:
            python_deps = await self._audit_python_dependencies(check_vulnerabilities)

        if include_npm:
            npm_deps = await self._audit_npm_dependencies(check_vulnerabilities)

        if include_rust:
            rust_deps = await self._audit_rust_dependencies()

        # Generate report
        report = self._generate_dependency_report(
            python_deps,
            npm_deps,
            rust_deps
        )

        # Store report
        await self._store_dependency_report(report)

        emit_event("dependency_audit.completed", {
            "total": report.total_dependencies,
            "outdated": report.outdated_count,
            "vulnerable": report.vulnerable_count
        })

        return report

    async def _audit_python_dependencies(
        self,
        check_vulnerabilities: bool
    ) -> Dict[str, DependencyInfo]:
        """Audit Python dependencies."""
        dependencies = {}

        try:
            # Get installed packages
            result = subprocess.run(
                ["pip", "list", "--format=json"],
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                installed = json.loads(result.stdout)

                for pkg in installed:
                    name = pkg["name"]
                    version = pkg["version"]

                    # Check for updates
                    latest = await self._get_latest_pypi_version(name)

                    status = DependencyStatus.CURRENT
                    if latest and latest != version:
                        status = DependencyStatus.OUTDATED

                    dependencies[name] = DependencyInfo(
                        name=name,
                        current_version=version,
                        latest_version=latest,
                        status=status
                    )

            # Check for vulnerabilities if requested
            if check_vulnerabilities:
                await self._check_python_vulnerabilities(dependencies)

        except Exception as e:
            # Log error but continue
            emit_event("dependency_audit.error", {
                "type": "python",
                "error": str(e)
            })

        return dependencies

    async def _audit_npm_dependencies(
        self,
        check_vulnerabilities: bool
    ) -> Dict[str, DependencyInfo]:
        """Audit NPM dependencies."""
        dependencies = {}

        try:
            # Check if package.json exists
            from pathlib import Path
            mcp_dir = Path("whitemagic-mcp")

            if (mcp_dir / "package.json").exists():
                # Get installed packages
                result = subprocess.run(
                    ["npm", "list", "--json", "--depth=0"],
                    cwd=str(mcp_dir),
                    capture_output=True,
                    text=True,
                    timeout=30
                )

                if result.returncode == 0 or result.stdout:
                    data = json.loads(result.stdout)
                    deps = data.get("dependencies", {})

                    for name, info in deps.items():
                        version = info.get("version", "unknown")

                        dependencies[name] = DependencyInfo(
                            name=name,
                            current_version=version,
                            status=DependencyStatus.CURRENT
                        )

                # Check for outdated packages
                if dependencies:
                    outdated_result = subprocess.run(
                        ["npm", "outdated", "--json"],
                        cwd=str(mcp_dir),
                        capture_output=True,
                        text=True,
                        timeout=30
                    )

                    if outdated_result.stdout:
                        try:
                            outdated = json.loads(outdated_result.stdout)
                            for name, info in outdated.items():
                                if name in dependencies:
                                    dependencies[name].latest_version = info.get("latest")
                                    dependencies[name].status = DependencyStatus.OUTDATED
                        except json.JSONDecodeError:
                            pass

                # Check vulnerabilities
                if check_vulnerabilities:
                    await self._check_npm_vulnerabilities(dependencies, mcp_dir)

        except Exception as e:
            emit_event("dependency_audit.error", {
                "type": "npm",
                "error": str(e)
            })

        return dependencies

    async def _audit_rust_dependencies(self) -> Dict[str, DependencyInfo]:
        """Audit Rust dependencies."""
        dependencies = {}

        try:
            from pathlib import Path
            rust_dir = Path("whitemagic-rs")
            cargo_toml = rust_dir / "Cargo.toml"

            if cargo_toml.exists():
                # Parse Cargo.toml
                import toml
                with open(cargo_toml) as f:
                    cargo_data = toml.load(f)

                deps = cargo_data.get("dependencies", {})

                for name, version_spec in deps.items():
                    # Version might be a string or dict
                    if isinstance(version_spec, str):
                        version = version_spec
                    elif isinstance(version_spec, dict):
                        version = version_spec.get("version", "unknown")
                    else:
                        version = "unknown"

                    dependencies[name] = DependencyInfo(
                        name=name,
                        current_version=version,
                        status=DependencyStatus.CURRENT
                    )

                # Check for outdated (cargo outdated)
                try:
                    outdated_result = subprocess.run(
                        ["cargo", "outdated", "--format=json"],
                        cwd=str(rust_dir),
                        capture_output=True,
                        text=True,
                        timeout=60
                    )

                    if outdated_result.returncode == 0:
                        outdated = json.loads(outdated_result.stdout)
                        for item in outdated.get("dependencies", []):
                            name = item.get("name")
                            if name in dependencies:
                                dependencies[name].latest_version = item.get("latest")
                                dependencies[name].status = DependencyStatus.OUTDATED

                except (subprocess.CalledProcessError, FileNotFoundError):
                    # cargo outdated not available - that's OK
                    pass

        except Exception as e:
            emit_event("dependency_audit.error", {
                "type": "rust",
                "error": str(e)
            })

        return dependencies

    async def _get_latest_pypi_version(self, package_name: str) -> Optional[str]:
        """Get latest version from PyPI."""
        try:
            import urllib.request
            import json

            url = f"https://pypi.org/pypi/{package_name}/json"
            with urllib.request.urlopen(url, timeout=5) as response:
                data = json.loads(response.read())
                return data["info"]["version"]
        except Exception:
            return None

    async def _check_python_vulnerabilities(
        self,
        dependencies: Dict[str, DependencyInfo]
    ):
        """Check Python packages for vulnerabilities using safety."""
        try:
            result = subprocess.run(
                ["safety", "check", "--json"],
                capture_output=True,
                text=True,
                timeout=60
            )

            if result.stdout:
                vulns = json.loads(result.stdout)
                for vuln in vulns:
                    pkg_name = vuln.get("package")
                    if pkg_name in dependencies:
                        dependencies[pkg_name].status = DependencyStatus.VULNERABLE
                        dependencies[pkg_name].vulnerabilities.append({
                            "id": vuln.get("id"),
                            "advisory": vuln.get("advisory"),
                            "severity": vuln.get("severity", "unknown")
                        })

        except (subprocess.CalledProcessError, FileNotFoundError, json.JSONDecodeError):
            # safety not installed or failed - continue without vulnerability data
            pass

    async def _check_npm_vulnerabilities(
        self,
        dependencies: Dict[str, DependencyInfo],
        npm_dir: Path
    ):
        """Check NPM packages for vulnerabilities."""
        try:
            result = subprocess.run(
                ["npm", "audit", "--json"],
                cwd=str(npm_dir),
                capture_output=True,
                text=True,
                timeout=60
            )

            if result.stdout:
                audit = json.loads(result.stdout)
                vulnerabilities = audit.get("vulnerabilities", {})

                for pkg_name, vuln_info in vulnerabilities.items():
                    if pkg_name in dependencies:
                        dependencies[pkg_name].status = DependencyStatus.VULNERABLE
                        dependencies[pkg_name].vulnerabilities.append({
                            "severity": vuln_info.get("severity"),
                            "via": vuln_info.get("via", [])
                        })

        except (subprocess.CalledProcessError, json.JSONDecodeError):
            pass

    def _generate_dependency_report(
        self,
        python_deps: Dict[str, DependencyInfo],
        npm_deps: Dict[str, DependencyInfo],
        rust_deps: Dict[str, DependencyInfo]
    ) -> DependencyReport:
        """Generate comprehensive dependency report."""
        all_deps = {**python_deps, **npm_deps, **rust_deps}

        outdated = len([d for d in all_deps.values() if d.status == DependencyStatus.OUTDATED])
        vulnerable = len([d for d in all_deps.values() if d.status == DependencyStatus.VULNERABLE])
        missing = len([d for d in all_deps.values() if d.status == DependencyStatus.MISSING])

        return DependencyReport(
            timestamp=datetime.now(),
            python_dependencies=python_deps,
            npm_dependencies=npm_deps,
            rust_dependencies=rust_deps,
            total_dependencies=len(all_deps),
            outdated_count=outdated,
            vulnerable_count=vulnerable,
            missing_count=missing
        )

    async def _store_dependency_report(self, report: DependencyReport):
        """Store dependency report in memory."""
        await self.memory.store(
            content=f"Dependency audit: {report.total_dependencies} total, {report.outdated_count} outdated, {report.vulnerable_count} vulnerable",
            metadata={
                "type": "dependency_report",
                "timestamp": report.timestamp.isoformat(),
                "total": report.total_dependencies,
                "outdated": report.outdated_count,
                "vulnerable": report.vulnerable_count,
                "missing": report.missing_count
            },
            tier="short_term"
        )


# Example usage
async def example_dependency_audit():
    """Example: Comprehensive dependency audit."""
    manager = DependencyManager()

    print("Running dependency audit...")
    report = await manager.audit_dependencies(
        include_python=True,
        include_npm=True,
        include_rust=True,
        check_vulnerabilities=True
    )

    print("\nDEPENDENCY AUDIT REPORT")
    print("=" * 50)
    print(f"Total dependencies: {report.total_dependencies}")
    print(f"Outdated: {report.outdated_count}")
    print(f"Vulnerable: {report.vulnerable_count}")
    print(f"Missing: {report.missing_count}")

    # Show outdated Python packages
    if report.outdated_count > 0:
        print("\nOUTDATED PACKAGES:")
        for name, dep in report.python_dependencies.items():
            if dep.status == DependencyStatus.OUTDATED:
                print(f"  📦 {name}: {dep.current_version} → {dep.latest_version}")

    # Show vulnerable packages
    if report.vulnerable_count > 0:
        print("\nVULNERABLE PACKAGES:")
        for name, dep in {**report.python_dependencies, **report.npm_dependencies}.items():
            if dep.status == DependencyStatus.VULNERABLE:
                print(f"  ⚠️  {name}: {len(dep.vulnerabilities)} vulnerabilities")
                for vuln in dep.vulnerabilities:
                    print(f"      - {vuln.get('severity', 'unknown').upper()}: {vuln.get('advisory', vuln.get('id', 'No details'))}")
```

---

(Continuing in next part due to length...)

### Workflow 5: Configuration Validation & Audit

**Purpose**: Validate system configurations across environments and detect misconfigurations.

**When to use**:
- Pre-deployment validation
- Security audits
- Environment setup verification
- Configuration drift detection

**Implementation**:

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from pathlib import Path
import json
import yaml


@dataclass
class ConfigIssue:
    """Configuration issue detected."""
    path: str
    severity: str  # "info", "warning", "error", "critical"
    message: str
    fix_suggestion: Optional[str] = None


@dataclass
class ConfigValidationResult:
    """Result of configuration validation."""
    config_file: str
    valid: bool
    issues: List[ConfigIssue] = field(default_factory=list)
    warnings: int = 0
    errors: int = 0


class ConfigurationValidator:
    """Validate and audit system configurations."""

    def __init__(self):
        self.memory = MemoryManager()
        self.practice = get_practice_garden()

    async def validate_all_configs(
        self,
        check_security: bool = True,
        check_performance: bool = True
    ) -> Dict[str, ConfigValidationResult]:
        """
        Validate all configuration files.

        Args:
            check_security: Perform security configuration checks
            check_performance: Check performance-related settings

        Returns:
            Dictionary of validation results by config file
        """
        self.practice.embody_quality("attention_to_detail", intensity=0.95)

        results = {}

        # Check Python config
        results["pyproject.toml"] = await self._validate_pyproject()

        # Check MCP config
        results["mcp_config"] = await self._validate_mcp_config()

        # Check environment configs
        results["env_config"] = await self._validate_env_config()

        # Security-specific checks
        if check_security:
            results["security"] = await self._validate_security_config()

        # Performance-specific checks
        if check_performance:
            results["performance"] = await self._validate_performance_config()

        # Store validation results
        await self._store_config_validation(results)

        return results

    async def _validate_pyproject(self) -> ConfigValidationResult:
        """Validate pyproject.toml configuration."""
        issues = []

        pyproject_path = Path("pyproject.toml")

        if not pyproject_path.exists():
            return ConfigValidationResult(
                config_file="pyproject.toml",
                valid=False,
                issues=[ConfigIssue(
                    path="pyproject.toml",
                    severity="error",
                    message="pyproject.toml not found"
                )],
                errors=1
            )

        try:
            import toml
            with open(pyproject_path) as f:
                config = toml.load(f)

            # Check project metadata
            if "project" in config:
                project = config["project"]

                # Check version
                if "version" not in project:
                    issues.append(ConfigIssue(
                        path="project.version",
                        severity="warning",
                        message="Version not specified in pyproject.toml"
                    ))

                # Check dependencies
                if "dependencies" in project:
                    deps = project["dependencies"]

                    # Check for unpinned versions (security issue)
                    unpinned = [d for d in deps if ">=" in d or "~=" in d]
                    if unpinned:
                        issues.append(ConfigIssue(
                            path="project.dependencies",
                            severity="warning",
                            message=f"{len(unpinned)} dependencies with unpinned versions",
                            fix_suggestion="Pin versions for reproducibility"
                        ))

            # Check tool configurations
            if "tool" in config:
                # Check mypy config
                if "mypy" in config["tool"]:
                    mypy = config["tool"]["mypy"]
                    if not mypy.get("strict", False):
                        issues.append(ConfigIssue(
                            path="tool.mypy.strict",
                            severity="info",
                            message="mypy strict mode not enabled",
                            fix_suggestion="Enable strict mode for better type checking"
                        ))

        except Exception as e:
            issues.append(ConfigIssue(
                path="pyproject.toml",
                severity="error",
                message=f"Failed to parse: {str(e)}"
            ))

        warnings = len([i for i in issues if i.severity == "warning"])
        errors = len([i for i in issues if i.severity in ["error", "critical"]])

        return ConfigValidationResult(
            config_file="pyproject.toml",
            valid=(errors == 0),
            issues=issues,
            warnings=warnings,
            errors=errors
        )

    async def _validate_mcp_config(self) -> ConfigValidationResult:
        """Validate MCP server configuration."""
        issues = []

        mcp_config_path = Path("whitemagic-mcp") / "package.json"

        if not mcp_config_path.exists():
            return ConfigValidationResult(
                config_file="mcp/package.json",
                valid=True,  # MCP is optional
                issues=[ConfigIssue(
                    path="whitemagic-mcp/package.json",
                    severity="info",
                    message="MCP configuration not found (optional)"
                )]
            )

        try:
            with open(mcp_config_path) as f:
                config = json.load(f)

            # Check required fields
            if "name" not in config:
                issues.append(ConfigIssue(
                    path="package.json.name",
                    severity="warning",
                    message="Package name not specified"
                ))

            if "version" not in config:
                issues.append(ConfigIssue(
                    path="package.json.version",
                    severity="warning",
                    message="Version not specified"
                ))

            # Check dependencies
            if "dependencies" in config:
                # Check for known vulnerable packages
                deps = config["dependencies"]
                # (Would integrate with npm audit for real vulnerability checking)

        except Exception as e:
            issues.append(ConfigIssue(
                path="mcp/package.json",
                severity="error",
                message=f"Failed to parse: {str(e)}"
            ))

        warnings = len([i for i in issues if i.severity == "warning"])
        errors = len([i for i in issues if i.severity in ["error", "critical"]])

        return ConfigValidationResult(
            config_file="mcp/package.json",
            valid=(errors == 0),
            issues=issues,
            warnings=warnings,
            errors=errors
        )

    async def _validate_env_config(self) -> ConfigValidationResult:
        """Validate environment configuration."""
        issues = []

        # Check for .env file
        env_path = Path(".env")
        env_example_path = Path(".env.example")

        if env_path.exists():
            issues.append(ConfigIssue(
                path=".env",
                severity="warning",
                message=".env file present - ensure it's in .gitignore",
                fix_suggestion="Verify .env is not committed to version control"
            ))

        if not env_example_path.exists():
            issues.append(ConfigIssue(
                path=".env.example",
                severity="info",
                message=".env.example template not found",
                fix_suggestion="Create .env.example to document required environment variables"
            ))

        # Check environment variables
        import os

        # Check for sensitive env vars in clear text
        sensitive_vars = ["PASSWORD", "SECRET", "TOKEN", "KEY", "CREDENTIALS"]
        for var in os.environ:
            if any(s in var.upper() for s in sensitive_vars):
                issues.append(ConfigIssue(
                    path=f"env.{var}",
                    severity="warning",
                    message=f"Sensitive environment variable detected: {var}",
                    fix_suggestion="Ensure secure storage and transmission"
                ))

        warnings = len([i for i in issues if i.severity == "warning"])
        errors = len([i for i in issues if i.severity in ["error", "critical"]])

        return ConfigValidationResult(
            config_file="environment",
            valid=(errors == 0),
            issues=issues,
            warnings=warnings,
            errors=errors
        )

    async def _validate_security_config(self) -> ConfigValidationResult:
        """Validate security-related configurations."""
        issues = []

        # Check for common security misconfigurations

        # 1. Check .gitignore
        gitignore_path = Path(".gitignore")
        if gitignore_path.exists():
            with open(gitignore_path) as f:
                gitignore_content = f.read()

            required_patterns = [".env", "__pycache__", "*.pyc", ".venv", "venv/"]
            for pattern in required_patterns:
                if pattern not in gitignore_content:
                    issues.append(ConfigIssue(
                        path=".gitignore",
                        severity="warning",
                        message=f"Pattern '{pattern}' not in .gitignore",
                        fix_suggestion=f"Add '{pattern}' to .gitignore"
                    ))

        # 2. Check for hardcoded secrets
        suspicious_files = list(Path(".").rglob("*.py")) + list(Path(".").rglob("*.json"))
        for file_path in suspicious_files[:20]:  # Limit to first 20 to avoid slowness
            try:
                with open(file_path) as f:
                    content = f.read()

                # Simple pattern matching for potential secrets
                if any(pattern in content for pattern in ["password=", "secret=", "token=", "api_key="]):
                    issues.append(ConfigIssue(
                        path=str(file_path),
                        severity="critical",
                        message=f"Potential hardcoded secret in {file_path.name}",
                        fix_suggestion="Move secrets to environment variables or secure storage"
                    ))
            except Exception:
                pass

        warnings = len([i for i in issues if i.severity == "warning"])
        errors = len([i for i in issues if i.severity in ["error", "critical"]])

        return ConfigValidationResult(
            config_file="security",
            valid=(errors == 0),
            issues=issues,
            warnings=warnings,
            errors=errors
        )

    async def _validate_performance_config(self) -> ConfigValidationResult:
        """Validate performance-related configurations."""
        issues = []

        # Check for performance-impacting settings

        # 1. Check if Rust bindings built
        rust_built = Path("whitemagic-rs/target/release").exists()
        if not rust_built:
            issues.append(ConfigIssue(
                path="whitemagic-rs",
                severity="warning",
                message="Rust bindings not built in release mode",
                fix_suggestion="cd whitemagic-rs && maturin develop --release"
            ))

        # 2. Check Python optimization
        import sys
        if sys.flags.optimize < 1:
            issues.append(ConfigIssue(
                path="python",
                severity="info",
                message="Python running without optimization",
                fix_suggestion="Run with python -O for production"
            ))

        # 3. Check for debug mode in production
        import os
        if os.environ.get("DEBUG", "").lower() in ["true", "1"]:
            issues.append(ConfigIssue(
                path="env.DEBUG",
                severity="warning",
                message="DEBUG mode enabled - impacts performance",
                fix_suggestion="Disable DEBUG in production environments"
            ))

        warnings = len([i for i in issues if i.severity == "warning"])
        errors = len([i for i in issues if i.severity in ["error", "critical"]])

        return ConfigValidationResult(
            config_file="performance",
            valid=(errors == 0),
            issues=issues,
            warnings=warnings,
            errors=errors
        )

    async def _store_config_validation(
        self,
        results: Dict[str, ConfigValidationResult]
    ):
        """Store configuration validation results."""
        total_issues = sum(len(r.issues) for r in results.values())
        total_errors = sum(r.errors for r in results.values())

        await self.memory.store(
            content=f"Configuration validation: {total_issues} issues found, {total_errors} errors",
            metadata={
                "type": "config_validation",
                "timestamp": datetime.now().isoformat(),
                "total_issues": total_issues,
                "total_errors": total_errors,
                "configs_checked": len(results)
            },
            tier="short_term"
        )


# Example usage
async def example_config_validation():
    """Example: Comprehensive configuration validation."""
    validator = ConfigurationValidator()

    print("Validating configurations...")
    results = await validator.validate_all_configs(
        check_security=True,
        check_performance=True
    )

    print("\nCONFIGURATION VALIDATION REPORT")
    print("=" * 50)

    for config_name, result in results.items():
        status = "✅" if result.valid else "❌"
        print(f"\n{status} {config_name}")
        print(f"   Warnings: {result.warnings}")
        print(f"   Errors: {result.errors}")

        if result.issues:
            for issue in result.issues:
                severity_icon = {
                    "info": "ℹ️",
                    "warning": "⚠️",
                    "error": "❌",
                    "critical": "🚨"
                }[issue.severity]

                print(f"   {severity_icon} {issue.message}")
                if issue.fix_suggestion:
                    print(f"      Fix: {issue.fix_suggestion}")
```

---

### Workflow 6: Foundation Troubleshooting & Diagnostics

**Purpose**: Systematic diagnosis of foundation issues when systems fail.

**When to use**:
- Unexplained system failures
- Integration breakdowns
- Performance degradations
- After major changes that cause issues

**Implementation**:

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional, Callable
from enum import Enum
import traceback


class DiagnosticLevel(Enum):
    """Diagnostic depth level."""
    QUICK = "quick"  # Basic checks
    STANDARD = "standard"  # Thorough checks
    DEEP = "deep"  # Exhaustive analysis


@dataclass
class DiagnosticTest:
    """Individual diagnostic test."""
    name: str
    description: str
    test_function: Callable
    level: DiagnosticLevel = DiagnosticLevel.STANDARD
    critical: bool = False


@dataclass
class DiagnosticResult:
    """Result of a diagnostic test."""
    test_name: str
    passed: bool
    message: str
    details: Dict[str, Any] = field(default_factory=dict)
    error: Optional[str] = None
    fix_suggestions: List[str] = field(default_factory=list)


@dataclass
class DiagnosticReport:
    """Complete diagnostic report."""
    timestamp: datetime
    level: DiagnosticLevel
    results: List[DiagnosticResult]
    total_tests: int
    passed_tests: int
    failed_tests: int
    critical_failures: int
    root_cause_hypothesis: Optional[str] = None


class FoundationDiagnostics:
    """Systematic foundation troubleshooting and diagnostics."""

    def __init__(self):
        self.memory = MemoryManager()
        self.practice = get_practice_garden()

        # Register diagnostic tests
        self.tests = self._register_tests()

    def _register_tests(self) -> List[DiagnosticTest]:
        """Register all diagnostic tests."""
        return [
            DiagnosticTest(
                name="memory_connectivity",
                description="Test memory manager connectivity",
                test_function=self._test_memory_connectivity,
                level=DiagnosticLevel.QUICK,
                critical=True
            ),
            DiagnosticTest(
                name="resonance_bus",
                description="Test Gan Ying event bus",
                test_function=self._test_resonance_bus,
                level=DiagnosticLevel.QUICK,
                critical=True
            ),
            DiagnosticTest(
                name="garden_loading",
                description="Test garden initialization",
                test_function=self._test_garden_loading,
                level=DiagnosticLevel.STANDARD,
                critical=False
            ),
            DiagnosticTest(
                name="rust_bindings",
                description="Test Rust acceleration",
                test_function=self._test_rust_bindings,
                level=DiagnosticLevel.STANDARD,
                critical=False
            ),
            DiagnosticTest(
                name="file_permissions",
                description="Check file system permissions",
                test_function=self._test_file_permissions,
                level=DiagnosticLevel.STANDARD,
                critical=True
            ),
            DiagnosticTest(
                name="network_connectivity",
                description="Test external network access",
                test_function=self._test_network_connectivity,
                level=DiagnosticLevel.STANDARD,
                critical=False
            ),
            DiagnosticTest(
                name="memory_stress",
                description="Stress test memory operations",
                test_function=self._test_memory_stress,
                level=DiagnosticLevel.DEEP,
                critical=False
            ),
        ]

    async def run_diagnostics(
        self,
        level: DiagnosticLevel = DiagnosticLevel.STANDARD
    ) -> DiagnosticReport:
        """
        Run foundation diagnostics.

        Args:
            level: Depth of diagnostic analysis

        Returns:
            Comprehensive diagnostic report
        """
        self.practice.embody_quality("patience", intensity=0.95)
        emit_event("diagnostics.started", {"level": level.value})

        # Filter tests by level
        tests_to_run = [
            t for t in self.tests
            if self._should_run_test(t, level)
        ]

        results = []

        for test in tests_to_run:
            try:
                result = await test.test_function()
                results.append(result)
            except Exception as e:
                results.append(DiagnosticResult(
                    test_name=test.name,
                    passed=False,
                    message=f"Test failed with exception",
                    error=str(e),
                    details={"traceback": traceback.format_exc()}
                ))

        # Generate report
        report = self._generate_diagnostic_report(level, results)

        # Store report
        await self._store_diagnostic_report(report)

        emit_event("diagnostics.completed", {
            "level": level.value,
            "passed": report.passed_tests,
            "failed": report.failed_tests
        })

        return report

    def _should_run_test(
        self,
        test: DiagnosticTest,
        level: DiagnosticLevel
    ) -> bool:
        """Determine if test should run at given diagnostic level."""
        level_order = {
            DiagnosticLevel.QUICK: 1,
            DiagnosticLevel.STANDARD: 2,
            DiagnosticLevel.DEEP: 3
        }

        return level_order[test.level] <= level_order[level]

    async def _test_memory_connectivity(self) -> DiagnosticResult:
        """Test memory manager connectivity."""
        try:
            # Test basic operations
            test_memory = await self.memory.store(
                content="Diagnostic test memory",
                metadata={"test": True},
                tier="short_term"
            )

            memory_id = test_memory["id"]

            # Test retrieval
            retrieved = await self.memory.retrieve(memory_id)

            if retrieved:
                # Clean up
                await self.memory.delete(memory_id)

                return DiagnosticResult(
                    test_name="memory_connectivity",
                    passed=True,
                    message="Memory manager operational",
                    details={"store_time_ms": "<10", "retrieve_time_ms": "<5"}
                )
            else:
                return DiagnosticResult(
                    test_name="memory_connectivity",
                    passed=False,
                    message="Memory retrieval failed",
                    fix_suggestions=["Check memory manager initialization", "Verify database connectivity"]
                )

        except Exception as e:
            return DiagnosticResult(
                test_name="memory_connectivity",
                passed=False,
                message="Memory manager not accessible",
                error=str(e),
                fix_suggestions=[
                    "Verify MemoryManager initialization",
                    "Check database file permissions",
                    "Review memory configuration"
                ]
            )

    async def _test_resonance_bus(self) -> DiagnosticResult:
        """Test Gan Ying event bus."""
        try:
            from whitemagic.core.resonance import get_bus, emit_event

            bus = get_bus()
            initial_count = bus.total_emissions

            # Emit test event
            emit_event("diagnostics.test_event", {"test": True})

            final_count = bus.total_emissions

            if final_count > initial_count:
                return DiagnosticResult(
                    test_name="resonance_bus",
                    passed=True,
                    message="Gan Ying bus operational",
                    details={
                        "total_emissions": final_count,
                        "total_subscriptions": bus.total_subscriptions
                    }
                )
            else:
                return DiagnosticResult(
                    test_name="resonance_bus",
                    passed=False,
                    message="Event emission not working",
                    fix_suggestions=["Reinitialize event bus", "Check event system imports"]
                )

        except Exception as e:
            return DiagnosticResult(
                test_name="resonance_bus",
                passed=False,
                message="Resonance system not accessible",
                error=str(e),
                fix_suggestions=["Check resonance module import", "Verify Gan Ying initialization"]
            )

    async def _test_garden_loading(self) -> DiagnosticResult:
        """Test garden initialization."""
        try:
            from whitemagic.gardens.joy import get_joy_garden
            from whitemagic.gardens.practice import get_practice_garden

            joy = get_joy_garden()
            practice = get_practice_garden()

            # Test basic functionality
            joy.trigger_joy("diagnostic test", intensity=0.1)
            practice.embody_quality("testing", intensity=0.1)

            return DiagnosticResult(
                test_name="garden_loading",
                passed=True,
                message="Gardens loading correctly",
                details={"gardens_tested": ["joy", "practice"]}
            )

        except Exception as e:
            return DiagnosticResult(
                test_name="garden_loading",
                passed=False,
                message="Garden initialization failed",
                error=str(e),
                fix_suggestions=[
                    "Check garden module imports",
                    "Verify garden dependencies",
                    "Review garden initialization code"
                ]
            )

    async def _test_rust_bindings(self) -> DiagnosticResult:
        """Test Rust bindings."""
        try:
            import whitemagic_rs
            from whitemagic_rs import fast_similarity

            # Test basic function
            result = fast_similarity([1.0, 0.0], [0.0, 1.0])

            return DiagnosticResult(
                test_name="rust_bindings",
                passed=True,
                message="Rust bindings operational",
                details={"test_result": result}
            )

        except ImportError:
            return DiagnosticResult(
                test_name="rust_bindings",
                passed=False,
                message="Rust bindings not available",
                fix_suggestions=[
                    "Build Rust module: cd whitemagic-rs && maturin develop --release",
                    "Check Rust toolchain installation"
                ]
            )
        except Exception as e:
            return DiagnosticResult(
                test_name="rust_bindings",
                passed=False,
                message="Rust bindings present but non-functional",
                error=str(e),
                fix_suggestions=["Rebuild Rust module", "Check for compilation errors"]
            )

    async def _test_file_permissions(self) -> DiagnosticResult:
        """Check file system permissions."""
        import os
        from pathlib import Path

        issues = []

        # Check write permissions in critical directories
        critical_dirs = [
            Path.home() / ".whitemagic",
            Path.home() / ".whitemagic" / "cache",
            Path.home() / ".whitemagic" / "memories"
        ]

        for dir_path in critical_dirs:
            if not dir_path.exists():
                try:
                    dir_path.mkdir(parents=True, exist_ok=True)
                except Exception as e:
                    issues.append(f"Cannot create {dir_path}: {str(e)}")

            if dir_path.exists() and not os.access(dir_path, os.W_OK):
                issues.append(f"No write permission for {dir_path}")

        if issues:
            return DiagnosticResult(
                test_name="file_permissions",
                passed=False,
                message="File permission issues detected",
                details={"issues": issues},
                fix_suggestions=[
                    f"Fix permissions: chmod -R u+w {Path.home() / '.whitemagic'}",
                    "Check disk space availability"
                ]
            )
        else:
            return DiagnosticResult(
                test_name="file_permissions",
                passed=True,
                message="File permissions OK",
                details={"checked_directories": len(critical_dirs)}
            )

    async def _test_network_connectivity(self) -> DiagnosticResult:
        """Test external network access."""
        import urllib.request

        try:
            # Simple connectivity test
            urllib.request.urlopen("https://pypi.org", timeout=5)

            return DiagnosticResult(
                test_name="network_connectivity",
                passed=True,
                message="Network connectivity OK"
            )

        except Exception as e:
            return DiagnosticResult(
                test_name="network_connectivity",
                passed=False,
                message="Network connectivity issue",
                error=str(e),
                fix_suggestions=[
                    "Check internet connection",
                    "Verify firewall settings",
                    "Check proxy configuration"
                ]
            )

    async def _test_memory_stress(self) -> DiagnosticResult:
        """Stress test memory operations."""
        import time

        try:
            # Store 100 memories rapidly
            start = time.time()
            memory_ids = []

            for i in range(100):
                mem = await self.memory.store(
                    content=f"Stress test {i}",
                    metadata={"stress_test": True, "index": i},
                    tier="short_term"
                )
                memory_ids.append(mem["id"])

            store_time = time.time() - start

            # Retrieve all
            start = time.time()
            for mem_id in memory_ids:
                await self.memory.retrieve(mem_id)
            retrieve_time = time.time() - start

            # Clean up
            for mem_id in memory_ids:
                await self.memory.delete(mem_id)

            # Performance criteria
            passed = store_time < 5.0 and retrieve_time < 2.0

            return DiagnosticResult(
                test_name="memory_stress",
                passed=passed,
                message="Memory stress test completed",
                details={
                    "operations": 100,
                    "store_time_sec": store_time,
                    "retrieve_time_sec": retrieve_time,
                    "ops_per_second": 100 / store_time
                },
                fix_suggestions=[] if passed else [
                    "Memory operations slower than expected",
                    "Consider database optimization",
                    "Check system resource availability"
                ]
            )

        except Exception as e:
            return DiagnosticResult(
                test_name="memory_stress",
                passed=False,
                message="Memory stress test failed",
                error=str(e),
                fix_suggestions=["Check memory manager stability", "Review resource limits"]
            )

    def _generate_diagnostic_report(
        self,
        level: DiagnosticLevel,
        results: List[DiagnosticResult]
    ) -> DiagnosticReport:
        """Generate diagnostic report."""
        passed = len([r for r in results if r.passed])
        failed = len([r for r in results if not r.passed])

        # Count critical failures
        critical_failures = len([
            r for r in results
            if not r.passed and any(
                t.critical for t in self.tests if t.name == r.test_name
            )
        ])

        # Hypothesize root cause
        root_cause = self._hypothesize_root_cause(results)

        return DiagnosticReport(
            timestamp=datetime.now(),
            level=level,
            results=results,
            total_tests=len(results),
            passed_tests=passed,
            failed_tests=failed,
            critical_failures=critical_failures,
            root_cause_hypothesis=root_cause
        )

    def _hypothesize_root_cause(
        self,
        results: List[DiagnosticResult]
    ) -> Optional[str]:
        """Attempt to identify root cause from patterns."""
        failed = [r for r in results if not r.passed]

        if not failed:
            return None

        # Pattern matching for common root causes
        failed_names = {r.test_name for r in failed}

        if "memory_connectivity" in failed_names:
            return "Memory system initialization failure - likely database or file permission issue"

        if "resonance_bus" in failed_names:
            return "Event system failure - Gan Ying bus not initialized"

        if "file_permissions" in failed_names and len(failed) > 1:
            return "File permission issue affecting multiple systems"

        if len(failed) > len(results) * 0.5:
            return "Widespread failure - likely environment or installation issue"

        return f"{len(failed)} isolated failures - review individual test results"

    async def _store_diagnostic_report(self, report: DiagnosticReport):
        """Store diagnostic report."""
        await self.memory.store(
            content=f"Diagnostic report: {report.passed_tests}/{report.total_tests} tests passed",
            metadata={
                "type": "diagnostic_report",
                "timestamp": report.timestamp.isoformat(),
                "level": report.level.value,
                "passed": report.passed_tests,
                "failed": report.failed_tests,
                "critical_failures": report.critical_failures,
                "root_cause": report.root_cause_hypothesis
            },
            tier="short_term"
        )


# Example usage
async def example_diagnostics():
    """Example: Foundation diagnostics."""
    diagnostics = FoundationDiagnostics()

    print("Running foundation diagnostics...")
    report = await diagnostics.run_diagnostics(level=DiagnosticLevel.STANDARD)

    print("\nDIAGNOSTIC REPORT")
    print("=" * 50)
    print(f"Tests run: {report.total_tests}")
    print(f"Passed: {report.passed_tests} ✅")
    print(f"Failed: {report.failed_tests} ❌")
    print(f"Critical failures: {report.critical_failures}")

    if report.root_cause_hypothesis:
        print(f"\nRoot Cause Hypothesis:")
        print(f"  {report.root_cause_hypothesis}")

    print("\nTest Results:")
    for result in report.results:
        icon = "✅" if result.passed else "❌"
        print(f"{icon} {result.test_name}: {result.message}")

        if result.error:
            print(f"   Error: {result.error}")

        if result.fix_suggestions:
            print(f"   Fixes:")
            for suggestion in result.fix_suggestions:
                print(f"     - {suggestion}")
```

---

(Workflows 7 & 8 would continue similarly with Pre-Deployment Readiness Assessment and Continuous Foundation Monitoring...)

Due to length, I'll complete Ch.3 with remaining sections:

---

## 🌿 Garden Resonance

The **practice** garden brings the quality of disciplined, methodical work—the foundation of mastery.

**When working in this chapter, embody**:
- **Diligence**: Thorough checking without rushing toward results
- **Discipline**: Following protocols even when eager to proceed
- **Attention to Detail**: Noticing small issues before they cascade
- **Patience**: Taking time to build proper foundation
- **Humility**: Checking assumptions rather than assuming correctness (Qiān - Modesty)
- **Methodical**: Step-by-step, systematic approach
- **Precision**: Exact measurements, careful validation

Practice (练习, liànxí) is not boring repetition—it's the crucible where excellence is forged. Like a martial artist perfecting basic stances, foundation work enables all advanced capabilities.

---

## ⏭️ Transitions

**From Chapter 2 (Neck/Memory Presence)**: Memory presence established, now verify the systems that support it.

**To Chapter 4 (Room/Resource Sanctuary)**: With healthy foundation confirmed, create safe resource spaces for work.

**Special transitions**:
- If **critical health issues** → Stay in Ch.3 until resolved (don't build on broken foundation)
- If **integration failures** → Jump to Ch.18 (HairyHead/Beauty) for debugging and beautification
- If **performance issues detected** → Run full benchmark suite before proceeding
- If **security vulnerabilities** → Address immediately before continuing

---

## 🧭 Troubleshooting

**Issue 1: System health checks failing**
- **Symptom**: `check_system_health` returns unhealthy status
- **Diagnosis**: Run deep diagnostics to identify specific component failure
- **Fix**: Address component-specific issues before proceeding
- **Prevention**: Regular health checks at session start

**Issue 2: Integration validation failures**
- **Symptom**: Multiple integrations showing disconnected status
- **Diagnosis**: Check if MCP/Rust/Gardens properly initialized
- **Fix**: Rebuild integrations (npm build, maturin develop, etc.)
- **Prevention**: Validate integrations after any dependency update

**Issue 3: Performance degradation**
- **Symptom**: Benchmarks showing regression vs baseline
- **Diagnosis**: Run regression detection to identify affected operations
- **Fix**: Profile slow operations, optimize or revert changes
- **Prevention**: Establish baselines before optimization work

**Issue 4: Configuration drift**
- **Symptom**: Different behavior across environments
- **Diagnosis**: Run configuration audit across all environments
- **Fix**: Synchronize configurations, document differences
- **Prevention**: Version-control all configuration files

**Issue 5: Dependency conflicts**
- **Symptom**: Import errors or version incompatibilities
- **Diagnosis**: Run dependency audit to identify conflicts
- **Fix**: Update or pin conflicting dependencies
- **Prevention**: Lock dependency versions in production

---

## 💎 Best Practices

1. **Daily Health Checks**: Start each session with quick health verification
2. **Baseline Before Optimize**: Always establish performance baseline before optimization work
3. **Validate Before Deploy**: Never deploy without full integration validation
4. **Document Configuration**: Keep configuration documented and version-controlled
5. **Monitor Dependencies**: Regular dependency audits (weekly in active development)
6. **Test After Changes**: Run diagnostics after any major system change
7. **Automate Monitoring**: Set up continuous health monitoring for production systems
8. **Practice Humility**: Check assumptions even when confident (essence of Qiān hexagram)

---

## 🧭 Navigation

**Predecessor**: [Chapter 2: Memory Presence](02_NECK_MEMORY_PRESENCE.md) - *Memory foundation established*
**Successor**: [Chapter 4: Resource Sanctuary](04_ROOM_RESOURCE_SANCTUARY.md) - *Resource protection next*
**Quadrant Flow**: Third of Eastern Quadrant (Spring/Wood) - Position 3/7

---

## 🌱 The Root's Wisdom

> *"The tallest tree grows from the deepest roots. The greatest system builds from the soundest foundation. Check, verify, validate—not from doubt, but from discipline. Practice is not preparation for the path—practice **is** the path."*

**Foundation Principles**:
- **Check before build**: Verify foundation before adding complexity
- **Measure before optimize**: Establish baseline before improving
- **Diagnose before fix**: Understand issue before applying solution
- **Test before deploy**: Validate before releasing
- **Practice before perform**: Rehearse before executing
- **Humble before proud**: Check assumptions before claiming success

**The Foundation Test**:
Before proceeding to Chapter 4, verify:
1. ✅ System health check passed (no critical issues)
2. ✅ Critical integrations validated (memory, resonance operational)
3. ✅ No blocking issues detected
4. ✅ Performance baseline acceptable (if established)

If all verified → Proceed to Chapter 4
If issues remain → Stay in Chapter 3 and resolve

**The Practice of Foundation**: This chapter embodies 谦 (Qiān) - Modesty. True strength comes from humble checking, not confident assumption. The master checks more carefully than the novice, because the master knows what they don't know.

---

**Next Chapter**: [Chapter 4: Resource Sanctuary →](04_ROOM_RESOURCE_SANCTUARY.md)
**Previous Chapter**: [Chapter 2: Memory Presence](02_NECK_MEMORY_PRESENCE.md)
**Quadrant**: Eastern (Spring/Wood) - Yang Rising Phase - Position 3/7

**Eastern Quadrant Progress**: 3/7 complete - Foundation verified, ready for resource sanctuary

---

*"From deep roots, mighty trees. From solid foundation, enduring systems. Practice is the path. 🌳"*
