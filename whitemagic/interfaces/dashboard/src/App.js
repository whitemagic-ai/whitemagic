import React, { useState, useEffect } from 'react';
import { QueryClient, QueryClientProvider, useQuery } from '@tanstack/react-query';
import { Brain, Network, Activity, Calendar, Search, Settings, Zap } from 'lucide-react';
import MemoryGraph from './components/MemoryGraph';
import Timeline from './components/Timeline';
import GardenHealth from './components/GardenHealth';
import SearchBar from './components/SearchBar';
import './App.css';

const queryClient = new QueryClient();

// API client
const api = {
  getMemories: async () => {
    const response = await fetch('/api/memories');
    return response.json();
  },
  getEvents: async () => {
    const response = await fetch('/api/events');
    return response.json();
  },
  getGardens: async () => {
    const response = await fetch('/api/gardens');
    return response.json();
  },
  search: async (query) => {
    const response = await fetch(`/api/search?q=${encodeURIComponent(query)}`);
    return response.json();
  }
};

function Dashboard() {
  const [selectedMemory, setSelectedMemory] = useState(null);
  const [searchQuery, setSearchQuery] = useState('');

  // Fetch data
  const { data: memories, isLoading: memoriesLoading } = useQuery({
    queryKey: ['memories'],
    queryFn: api.getMemories,
    refetchInterval: 30000 // Refresh every 30 seconds
  });

  const { data: events, isLoading: eventsLoading } = useQuery({
    queryKey: ['events'],
    queryFn: api.getEvents,
    refetchInterval: 10000 // Refresh every 10 seconds
  });

  const { data: gardens, isLoading: gardensLoading } = useQuery({
    queryKey: ['gardens'],
    queryFn: api.getGardens,
    refetchInterval: 60000 // Refresh every minute
  });

  const stats = {
    totalMemories: memories?.length || 0,
    recentEvents: events?.slice(0, 5) || [],
    activeGardens: gardens?.filter(g => g.status === 'active').length || 0,
    totalGardens: gardens?.length || 0
  };

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <header className="bg-white shadow-sm border-b border-gray-200">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center h-16">
            <div className="flex items-center space-x-3">
              <Brain className="w-8 h-8 text-purple-600" />
              <h1 className="text-xl font-bold text-gray-900">WhiteMagic Dashboard</h1>
            </div>
            <div className="flex items-center space-x-4">
              <SearchBar onSearch={setSearchQuery} />
              <button className="p-2 text-gray-500 hover:text-gray-700">
                <Settings className="w-5 h-5" />
              </button>
            </div>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Stats Cards */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8">
          <StatCard
            icon={Brain}
            label="Total Memories"
            value={stats.totalMemories}
            loading={memoriesLoading}
            color="purple"
          />
          <StatCard
            icon={Network}
            label="Active Gardens"
            value={`${stats.activeGardens}/${stats.totalGardens}`}
            loading={gardensLoading}
            color="blue"
          />
          <StatCard
            icon={Activity}
            label="Recent Events"
            value={stats.recentEvents.length}
            loading={eventsLoading}
            color="green"
          />
          <StatCard
            icon={Zap}
            label="System Health"
            value="98%"
            loading={false}
            color="yellow"
          />
        </div>

        {/* Main Grid */}
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Memory Graph - 2 columns */}
          <div className="lg:col-span-2">
            <div className="bg-white rounded-lg shadow p-6">
              <h2 className="text-lg font-semibold text-gray-900 mb-4 flex items-center">
                <Network className="w-5 h-5 mr-2 text-purple-600" />
                Memory Network
              </h2>
              <MemoryGraph
                memories={memories || []}
                selectedMemory={selectedMemory}
                onMemorySelect={setSelectedMemory}
                loading={memoriesLoading}
              />
            </div>
          </div>

          {/* Side Panel */}
          <div className="space-y-6">
            {/* Garden Health */}
            <div className="bg-white rounded-lg shadow p-6">
              <h2 className="text-lg font-semibold text-gray-900 mb-4">Garden Health</h2>
              <GardenHealth gardens={gardens || []} loading={gardensLoading} />
            </div>

            {/* Recent Events */}
            <div className="bg-white rounded-lg shadow p-6">
              <h2 className="text-lg font-semibold text-gray-900 mb-4 flex items-center">
                <Calendar className="w-5 h-5 mr-2 text-blue-600" />
                Recent Events
              </h2>
              <Timeline events={stats.recentEvents} loading={eventsLoading} />
            </div>
          </div>
        </div>

        {/* Selected Memory Detail */}
        {selectedMemory && (
          <div className="mt-8 bg-white rounded-lg shadow p-6">
            <h2 className="text-lg font-semibold text-gray-900 mb-4">Memory Details</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <h3 className="text-sm font-medium text-gray-500">Title</h3>
                <p className="mt-1 text-sm text-gray-900">{selectedMemory.title}</p>
              </div>
              <div>
                <h3 className="text-sm font-medium text-gray-500">Created</h3>
                <p className="mt-1 text-sm text-gray-900">
                  {new Date(selectedMemory.created_at).toLocaleString()}
                </p>
              </div>
              <div className="md:col-span-2">
                <h3 className="text-sm font-medium text-gray-500">Content</h3>
                <p className="mt-1 text-sm text-gray-900 whitespace-pre-wrap">
                  {selectedMemory.content}
                </p>
              </div>
            </div>
          </div>
        )}
      </main>
    </div>
  );
}

// Stat Card Component
function StatCard({ icon: Icon, label, value, loading, color }) {
  const colorClasses = {
    purple: 'text-purple-600 bg-purple-100',
    blue: 'text-blue-600 bg-blue-100',
    green: 'text-green-600 bg-green-100',
    yellow: 'text-yellow-600 bg-yellow-100'
  };

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <div className="flex items-center justify-between">
        <div>
          <p className="text-sm font-medium text-gray-600">{label}</p>
          <p className="mt-2 text-3xl font-bold text-gray-900">
            {loading ? '...' : value}
          </p>
        </div>
        <div className={`p-3 rounded-lg ${colorClasses[color]}`}>
          <Icon className="w-6 h-6" />
        </div>
      </div>
    </div>
  );
}

// Wrapped app with query client
function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <Dashboard />
    </QueryClientProvider>
  );
}

export default App;
