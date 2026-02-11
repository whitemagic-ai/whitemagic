import React from 'react';
import { Calendar, Clock, Zap, Brain, Heart } from 'lucide-react';

const Timeline = ({ events, loading }) => {
  const getEventIcon = (type) => {
    const icons = {
      memory_created: Brain,
      memory_updated: Clock,
      garden_activated: Heart,
      synergy_triggered: Zap,
      default: Calendar
    };
    const Icon = icons[type] || icons.default;
    return <Icon className="w-4 h-4" />;
  };

  const getEventColor = (type) => {
    const colors = {
      memory_created: 'text-purple-600 bg-purple-100',
      memory_updated: 'text-blue-600 bg-blue-100',
      garden_activated: 'text-green-600 bg-green-100',
      synergy_triggered: 'text-yellow-600 bg-yellow-100',
      default: 'text-gray-600 bg-gray-100'
    };
    return colors[type] || colors.default;
  };

  if (loading) {
    return (
      <div className="space-y-4">
        {[1, 2, 3].map(i => (
          <div key={i} className="animate-pulse">
            <div className="h-4 bg-gray-200 rounded w-3/4 mb-2"></div>
            <div className="h-3 bg-gray-200 rounded w-1/2"></div>
          </div>
        ))}
      </div>
    );
  }

  if (!events.length) {
    return (
      <p className="text-gray-500 text-sm">No recent events</p>
    );
  }

  return (
    <div className="space-y-4">
      {events.map((event, index) => (
        <div key={event.id || index} className="flex items-start space-x-3">
          <div className={`p-2 rounded-full ${getEventColor(event.type)}`}>
            {getEventIcon(event.type)}
          </div>
          <div className="flex-1 min-w-0">
            <p className="text-sm font-medium text-gray-900 truncate">
              {event.title || event.type.replace('_', ' ')}
            </p>
            <p className="text-xs text-gray-500">
              {new Date(event.timestamp).toLocaleTimeString()}
            </p>
            {event.description && (
              <p className="text-xs text-gray-600 mt-1">
                {event.description}
              </p>
            )}
          </div>
        </div>
      ))}
    </div>
  );
};

export default Timeline;
