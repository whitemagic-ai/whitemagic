"""Tests for incremental community maintenance module."""


class TestCommunity:
    """Test Community dataclass."""

    def test_to_dict(self):
        from whitemagic.core.intelligence.community_maintenance import Community

        c = Community(community_id="c1", label="test", member_count=5)
        d = c.to_dict()
        assert d["community_id"] == "c1"
        assert d["member_count"] == 5


class TestCommunityMaintainer:
    """Test the incremental community maintenance system."""

    def test_propagate_no_neighbors(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        result = m.propagate_label("mem1", neighbors=[])
        assert result.action == "no_neighbors"
        assert result.assigned_community is None

    def test_propagate_weak_neighbors(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        # Neighbors with weight below MIN_PROPAGATION_WEIGHT
        result = m.propagate_label("mem1", neighbors=[("n1", 0.1), ("n2", 0.1)])
        assert result.action == "no_neighbors"

    def test_propagate_creates_new_community(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        # Strong neighbors but none have communities yet
        result = m.propagate_label(
            "mem1",
            neighbors=[("n1", 0.8), ("n2", 0.7)],
            memory_tags=["python", "architecture"],
        )
        assert result.action == "created_new"
        assert result.assigned_community is not None

    def test_propagate_assigns_existing_community(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        # Create a community first
        r1 = m.propagate_label("n1", neighbors=[("n2", 0.8), ("n3", 0.7)], memory_tags=["python"])
        community_id = r1.assigned_community

        # Now assign n2 to the same community (it's a member via n1's community)
        m._member_to_community["n2"] = community_id
        m._member_to_community["n3"] = community_id

        # New memory with neighbors in that community
        r2 = m.propagate_label("mem_new", neighbors=[("n2", 0.9), ("n3", 0.8)])
        assert r2.action == "assigned"
        assert r2.assigned_community == community_id

    def test_get_community(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        r = m.propagate_label("mem1", neighbors=[("n1", 0.8), ("n2", 0.7)], memory_tags=["test"])
        community = m.get_community("mem1")
        assert community is not None
        assert community.community_id == r.assigned_community

    def test_merge_communities(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        r1 = m.propagate_label("a1", neighbors=[("x1", 0.8), ("x2", 0.7)], memory_tags=["group_a"])
        r2 = m.propagate_label("b1", neighbors=[("y1", 0.8), ("y2", 0.7)], memory_tags=["group_b"])

        survivor = m.merge_communities(r1.assigned_community, r2.assigned_community)
        assert survivor is not None
        # Both should now be in the same community
        assert m.get_community("a1").community_id == m.get_community("b1").community_id

    def test_check_health(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        m.propagate_label("a", neighbors=[("n1", 0.8), ("n2", 0.7)], memory_tags=["x"])
        health = m.check_health()
        assert "total_communities" in health
        assert "total_members" in health

    def test_import_batch_communities(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        assignments = {"mem1": 0, "mem2": 0, "mem3": 1, "mem4": 1, "mem5": 2}
        labels = {0: "Python", 1: "Architecture", 2: "Testing"}
        count = m.import_batch_communities(assignments, labels)
        assert count == 5
        assert m.get_community("mem1") is not None
        assert m.get_community("mem1").label == "Python"

    def test_get_status(self):
        from whitemagic.core.intelligence.community_maintenance import CommunityMaintainer

        m = CommunityMaintainer()
        m.propagate_label("a", neighbors=[("n1", 0.8), ("n2", 0.7)], memory_tags=["x"])
        status = m.get_status()
        assert status["total_communities"] >= 1
        assert "total_propagations" in status

    def test_singleton(self):
        from whitemagic.core.intelligence.community_maintenance import get_community_maintainer

        m1 = get_community_maintainer()
        m2 = get_community_maintainer()
        assert m1 is m2
