# 28 Gana → Garden Canonical Map

> Unified mapping between the 28 Lunar Mansion PRAT tools (Ganas) and their
> corresponding Garden implementations. Each Garden provides the stateful
> substrate that its Gana's tools operate on.

## Eastern Quadrant — Azure Dragon (Spring)

| # | Mansion | Gana | Garden | Category | Key Methods |
|---|---------|------|--------|----------|-------------|
| 1 | Horn 角 Jiao | `gana_horn` | **courage** | initialization | `create_session`, `resume_session`, `checkpoint_session`, `end_session` |
| 2 | Neck 亢 Kang | `gana_neck` | **practice** | memory_ops | Memory CRUD, context management |
| 3 | Root 氐 Di | `gana_root` | **truth** | health | `check_accelerators`, `health_report`, `ship_check`, `get_state_paths` |
| 4 | Room 房 Fang | `gana_room` | **sanctuary** | isolation | `set_limits`, `acquire_lock`, `release_lock`, `record_violation` |
| 5 | Heart 心 Xin | `gana_heart` | **love** | context | Session context, vital connection |
| 6 | Tail 尾 Wei | `gana_tail` | **metal** | acceleration | Wu Xing cycles, zodiac cores, performance |
| 7 | Winnowing 箕 Ji | `gana_winnowing_basket` | **wisdom** | search | Semantic search, filtering, extraction |

## Southern Quadrant — Vermilion Bird (Summer)

| # | Mansion | Gana | Garden | Category | Key Methods |
|---|---------|------|--------|----------|-------------|
| 8 | Ghost 鬼 Gui | `gana_ghost` | **grief** | introspection | `record_shadow`, `take_snapshot`, `get_trend`, `get_emotional_context` |
| 9 | Willow 柳 Liu | `gana_willow` | **play** | resilience | Flexibility, adaptive response |
| 10 | Star 星 Xing | `gana_star` | **beauty** | governance | PRAT illumination, governance beauty |
| 11 | Extended Net 张 Zhang | `gana_extended_net` | **connection** | connectivity | Pattern connectivity, association networks |
| 12 | Wings 翼 Yi | `gana_wings` | **adventure** | portability | `record_export`, `record_import`, `record_deploy_check` |
| 13 | Chariot 轸 Zhen | `gana_chariot` | **transformation** | archaeology | `record_dig`, `record_navigation`, `suggest_refactor` |
| 14 | Abundance 豐 Feng | `gana_abundance` | **joy** | regeneration | Dream cycle, regeneration |

## Western Quadrant — White Tiger (Autumn)

| # | Mansion | Gana | Garden | Category | Key Methods |
|---|---------|------|--------|----------|-------------|
| 15 | Straddling 奎 Kui | `gana_straddling_legs` | **patience** | ethics | `evaluate_action`, `check_boundary`, `verify_consent`, `get_ethical_score` |
| 16 | Mound 娄 Lou | `gana_mound` | **gratitude** | metrics | `track_metric`, `get_metrics_summary`, `record_yin_yang`, `get_yin_yang_balance` |
| 17 | Stomach 胃 Wei | `gana_stomach` | **healing** | memory_lifecycle | `record_sweep`, `record_consolidation`, `record_archive` |
| 18 | Hairy Head 昴 Mao | `gana_hairy_head` | **presence** | debugging | Detail, debugging, mindful attention |
| 19 | Net 毕 Bi | `gana_net` | **mystery** | pattern_capture | `record_pattern`, `record_mining_run`, `record_constellation` |
| 20 | Turtle Beak 觜 Zui | `gana_turtle_beak` | **voice** | validation | Precision validation, assertion |
| 21 | Three Stars 参 Shen | `gana_three_stars` | **dharma** | judgment | Wisdom council, dharma evaluation |

## Northern Quadrant — Black Tortoise (Winter)

| # | Mansion | Gana | Garden | Category | Key Methods |
|---|---------|------|--------|----------|-------------|
| 22 | Dipper 斗 Dou | `gana_dipper` | **awe** | governance | `toggle_prat_mode`, `get_prat_status`, `suggest_strategy`, `record_decision` |
| 23 | Ox 牛 Niu | `gana_ox` | **reverence** | endurance | `decompose_task`, `route_subtask`, `complete_subtask`, `cast_vote`, `resolve_vote` |
| 24 | Girl 女 Nü | `gana_girl` | **wonder** | nurture | Agent profiles, adaptive learning |
| 25 | Void 虚 Xu | `gana_void` | **stillness** | garden_ops | `list_gardens`, `get_garden_status`, `galactic_dashboard`, `record_defrag` |
| 26 | Roof 危 Wei | `gana_roof` | **protection** | safety | Shelter, safety enforcement |
| 27 | Encampment 室 Shi | `gana_encampment` | **sangha** | community | Session handoff, archive structure |
| 28 | Wall 壁 Bi | `gana_wall` | **air** | boundaries | Notifications, defense, filtering |

## Cross-Cutting Gardens (not mansion-mapped)

| Garden | Purpose |
|--------|---------|
| **browser** | Browser automation tooling |
| **creation** | Creative/generative cross-concern |
| **humor** | Levity cross-concern |

## Architecture Notes

- Each Garden inherits from `BaseGarden` + `GanYingMixin`
- Every Garden has: `mansion_number`, `gana_name`, `category`, `resonance_partners`
- Every Garden provides: `get_name()`, `get_coordinate_bias()`, `get_status()`
- Gardens are thread-safe (threading.Lock) for concurrent agent access
- Gan Ying events flow between gardens via `@listen_for` decorators
- PRAT resonance metadata in `prat_resonance.py` matches garden names exactly

## Source Files

- Gana classes: `whitemagic/core/ganas/{eastern,southern,western,northern}_quadrant.py`
- Gana registry: `whitemagic/core/ganas/registry.py`
- PRAT metadata: `whitemagic/tools/prat_resonance.py`
- PRAT router: `whitemagic/tools/prat_router.py`
- Garden base: `whitemagic/gardens/base_garden.py`
- Gardens: `whitemagic/gardens/{name}/__init__.py`
