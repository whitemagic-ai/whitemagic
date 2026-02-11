# WhiteMagic Dashboard

A modern React-based dashboard for visualizing and managing WhiteMagic memories, gardens, and system activity.

## Features

- **Memory Network Visualization**: Interactive D3.js force-directed graph of memory relationships
- **Real-time Updates**: Live data refresh with WebSocket support
- **Garden Health Monitoring**: Visual health indicators for all 14 gardens
- **Event Timeline**: Track system events and activities
- **Search Functionality**: Quick search across memories, gardens, and events
- **Responsive Design**: Works on desktop and mobile devices

## Quick Start

### Prerequisites

- Node.js 16+ and npm
- Python 3.8+
- WhiteMagic installed

### Installation

1. Install dependencies:
```bash
cd whitemagic/dashboard
npm install
```

2. Start the backend server:
```bash
python3 server.py --port 3001
```

3. In a new terminal, start the frontend:
```bash
npm start
```

4. Open http://localhost:3000 in your browser

## Development

### Project Structure

```
dashboard/
├── public/           # Static assets
├── src/
│   ├── components/  # React components
│   │   ├── MemoryGraph.js    # D3.js memory visualization
│   │   ├── Timeline.js       # Event timeline
│   │   ├── GardenHealth.js   # Garden status
│   │   └── SearchBar.js      # Search component
│   ├── App.js       # Main application
│   └── index.js     # Entry point
├── server.py        # Flask API server
└── package.json     # Dependencies
```

### API Endpoints

- `GET /api/memories` - Get all memories with relationships
- `GET /api/events` - Get recent system events
- `GET /api/gardens` - Get garden status and health
- `GET /api/search?q=query` - Search across all data
- `GET /api/plugins` - Get loaded plugins
- `GET /api/stats` - Get system statistics

### Customization

#### Adding New Components

1. Create component in `src/components/`
2. Import in `App.js`
3. Add to dashboard layout

#### Styling

Uses Tailwind CSS for styling. Customize colors in `tailwind.config.js`.

#### Data Sources

Modify `server.py` to connect to different data sources or add new endpoints.

## Production Deployment

### Build Frontend

```bash
npm run build
```

### Deploy with Docker

```dockerfile
FROM node:16-alpine as frontend
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY . .
RUN npm run build

FROM python:3.9-slim as backend
WORKDIR /app
COPY --from=frontend /app/build ./static
COPY server.py .
RUN pip install flask flask-cors
CMD ["python", "server.py"]
```

### Environment Variables

- `REACT_APP_API_URL` - Backend API URL (default: http://localhost:3001)
- `PORT` - Frontend port (default: 3000)

## Contributing

1. Fork the repository
2. Create feature branch
3. Make changes
4. Add tests
5. Submit pull request

## License

MIT License - see LICENSE file for details
