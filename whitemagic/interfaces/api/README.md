# WhiteMagic REST API

Production-ready REST API for the WhiteMagic memory management system.

## Quick Start

### 1. Install Dependencies

```bash
pip install -r requirements-api.txt
```

### 2. Set Environment Variables

```bash
export DATABASE_URL="postgresql+asyncpg://user:pass@localhost/whitemagic"
export WM_BASE_PATH="/path/to/whitemagic"
export ALLOWED_ORIGINS="https://app.whitemagic.dev,http://localhost:3000"
```

### 3. Run the API

```bash
# Development
python3 -m whitemagic.interfaces.api.app

# Production (with Uvicorn)
uvicorn whitemagic.interfaces.api.app:app --host 0.0.0.0 --port 8000 --workers 4
```

### 4. Access Documentation

- **Swagger UI**: <http://localhost:8000/docs>
- **ReDoc**: <http://localhost:8000/redoc>
- **OpenAPI JSON**: <http://localhost:8000/openapi.json>

---

## Authentication

All API endpoints (except `/health`) require authentication via API key.

### Headers

```http
Authorization: Bearer wm_prod_xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

### Getting an API Key

API keys are managed through the Whop integration or can be created programmatically:

```python
from whitemagic.interfaces.api.database import Database
from whitemagic.interfaces.api.auth import create_api_key

db = Database(DATABASE_URL)
async with db.get_session() as session:
    raw_key, api_key = await create_api_key(
        session,
        user_id=user.id,
        name="Production Key"
    )
    print(f"API Key: {raw_key}")  # Show to user ONCE!
```

---

## API Endpoints

### Health Check

```http
GET /health
```

No authentication required.

**Response:**

```json
{
  "status": "healthy",
  "version": "3.0.0"
}
```

---

### Memory Management

#### Create Memory

```http
POST /api/v1/memories
Authorization: Bearer {api_key}
Content-Type: application/json

{
  "title": "API Design Pattern",
  "content": "Always validate inputs at API boundaries...",
  "type": "long_term",
  "tags": ["api", "pattern", "proven"]
}
```

**Response:**

```json
{
  "success": true,
  "filename": "20251102_094530_api_design_pattern.md",
  "title": "API Design Pattern",
  "type": "long_term",
  "tags": ["api", "pattern", "proven"],
  "created": "2025-11-02T09:45:30",
  "path": "memory/long_term/20251102_094530_api_design_pattern.md"
}
```

#### List Memories

```http
GET /api/v1/memories?type=long_term
Authorization: Bearer {api_key}
```

**Response:**

```json
{
  "success": true,
  "total": 5,
  "memories": [...]
}
```

#### Get Memory

```http
GET /api/v1/memories/{filename}
Authorization: Bearer {api_key}
```

#### Update Memory

```http
PUT /api/v1/memories/{filename}
Authorization: Bearer {api_key}
Content-Type: application/json

{
  "title": "Updated Title",
  "tags": ["updated", "tags"]
}
```

#### Delete Memory

```http
DELETE /api/v1/memories/{filename}
Authorization: Bearer {api_key}
```

---

### Search

```http
POST /api/v1/search
Authorization: Bearer {api_key}
Content-Type: application/json

{
  "query": "API design",
  "tags": ["pattern"],
  "type": "long_term",
  "limit": 50
}
```

**Response:**

```json
{
  "success": true,
  "total": 3,
  "query": "API design",
  "results": [
    {
      "filename": "...",
      "title": "API Design Pattern",
      "type": "long_term",
      "tags": ["api", "pattern"],
      "created": "2025-11-02T09:45:30",
      "preview": "Always validate inputs...",
      "score": 5
    }
  ]
}
```

---

### Context Generation

```http
POST /api/v1/context
Authorization: Bearer {api_key}
Content-Type: application/json

{
  "tier": 1
}
```

Tiers:

- `0`: Minimal (titles only)
- `1`: Balanced (titles + previews) - **recommended**
- `2`: Full (complete content)

**Response:**

```json
{
  "success": true,
  "tier": 1,
  "memories_included": 7,
  "context": "## Short-Term Memories\n\n- **API Design**..."
}
```

---

### Consolidation

```http
POST /api/v1/consolidate
Authorization: Bearer {api_key}
Content-Type: application/json

{
  "dry_run": true,
  "min_age_days": 30
}
```

Archives old short-term memories. Memories with `proven` tag are promoted to long-term.

---

### Statistics

#### Get Stats

```http
GET /api/v1/stats
Authorization: Bearer {api_key}
```

**Response:**

```json
{
  "success": true,
  "short_term_count": 5,
  "long_term_count": 12,
  "total_count": 17,
  "total_tags": 8,
  "most_used_tags": [["api", 5], ["pattern", 3]]
}
```

#### List Tags

```http
GET /api/v1/tags
Authorization: Bearer {api_key}
```

---

### User Information

```http
GET /api/v1/user/me
Authorization: Bearer {api_key}
```

**Response:**

```json
{
  "success": true,
  "user": {
    "id": "uuid",
    "email": "user@example.com",
    "plan_tier": "pro",
    "created_at": "2025-11-01T10:00:00"
  },
  "usage": {
    "requests_today": 45,
    "requests_this_month": 1203,
    "memories_count": 17,
    "storage_bytes": 51200
  }
}
```

---

## Error Handling

All errors follow a consistent format:

```json
{
  "success": false,
  "error": {
    "code": "HTTP_404",
    "message": "Memory not found: example.md",
    "field": null
  }
}
```

### Common Error Codes

| Code | Status | Description |
|------|--------|-------------|
| `HTTP_400` | 400 | Bad Request - Invalid input |
| `HTTP_401` | 401 | Unauthorized - Invalid/missing API key |
| `HTTP_404` | 404 | Not Found - Resource doesn't exist |
| `HTTP_422` | 422 | Validation Error - Invalid request data |
| `HTTP_429` | 429 | Rate Limit Exceeded |
| `HTTP_500` | 500 | Internal Server Error |

---

## Rate Limiting

Rate limits are enforced per API key based on plan tier:

| Plan | Requests/Minute | Requests/Day |
|------|-----------------|--------------|
| Free | 10 | 100 |
| Starter | 60 | 5,000 |
| Pro | 300 | 50,000 |
| Enterprise | 1,000 | 1,000,000 |

Rate limit headers are included in responses:

```
X-RateLimit-Limit: 60
X-RateLimit-Remaining: 45
X-RateLimit-Reset: 1699012345
```

---

## Database Schema

See [database.py](database.py) for complete schema.

**Key tables:**

- `users` - User accounts
- `api_keys` - API key authentication
- `usage_records` - Request analytics
- `quotas` - Usage limits

---

## Development

### Running Tests

```bash
# All API tests
pytest tests/test_api_*.py -v

# Specific test file
pytest tests/test_api_endpoints.py -v

# With coverage
pytest tests/test_api_*.py --cov=whitemagic.interfaces.api --cov-report=html
```

### Database Migrations

Using Alembic (to be set up in Day 3):

```bash
# Create migration
alembic revision --autogenerate -m "Add new field"

# Apply migration
alembic upgrade head

# Rollback
alembic downgrade -1
```

---

## Deployment

### Environment Variables

```bash
# Required
DATABASE_URL=postgresql+asyncpg://user:pass@host/db
WM_BASE_PATH=/app/whitemagic

# Optional
ALLOWED_ORIGINS=https://app.whitemagic.dev,https://dashboard.whitemagic.dev
SENTRY_DSN=https://...  # For error tracking
REDIS_URL=redis://localhost:6379  # For rate limiting (Day 3)
```

### Docker

```dockerfile
FROM python:3.11-slim

WORKDIR /app
COPY requirements-api.txt .
RUN pip install -r requirements-api.txt

COPY whitemagic/ ./whitemagic/
COPY cli.py .

CMD ["uvicorn", "whitemagic.interfaces.api.app:app", "--host", "0.0.0.0", "--port", "8000"]
```

### Production Checklist

- [ ] Set strong `DATABASE_URL` with SSL
- [ ] Configure `ALLOWED_ORIGINS` for CORS
- [ ] (Optional) Set up error tracking (Sentry)
- [ ] Enable rate limiting (Redis)
- [ ] Configure logging
- [ ] Set up database backups
- [ ] Enable HTTPS (TLS termination)
- [ ] Configure health checks
- [ ] Set up monitoring/alerts

---

## Architecture

```
┌─────────────────────────────────────────┐
│          FastAPI Application            │
├─────────────────────────────────────────┤
│  - Request Validation (Pydantic)        │
│  - Authentication (API Keys)            │
│  - Rate Limiting (Redis)                │
│  - Error Handling                       │
└────────────┬─────────────┬──────────────┘
             │             │
    ┌────────▼────────┐   ┌▼──────────────┐
    │  WhiteMagic     │   │  PostgreSQL   │
    │  Core Library   │   │  Database     │
    │  (unchanged)    │   │  - Users      │
    └─────────────────┘   │  - API Keys   │
                          │  - Usage      │
                          │  - Quotas     │
                          └───────────────┘
```

The API layer wraps the existing WhiteMagic core library without modifying it.

---

## Next Steps

- **Day 3**: Rate limiting with Redis
- **Day 4**: Whop integration for payments
- **Day 5**: User dashboard
- **Day 6**: Observability (logging, metrics, monitoring)
- **Day 7**: Testing and production launch

---

## Support

- **Documentation**: Full API docs at `/docs` endpoint
- **Issues**: <https://github.com/whitemagic-ai/whitemagic/issues>
- **Email**: <support@whitemagic.dev> (coming soon)
