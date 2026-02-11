import React from 'react';
import { Flower, Tree, Sun, Droplets, Wind, Mountain } from 'lucide-react';

const GardenHealth = ({ gardens, loading }) => {
  const getGardenIcon = (name) => {
    const icons = {
      joy: Flower,
      love: Heart,
      truth: Sun,
      wisdom: Tree,
      practice: Droplets,
      presence: Wind,
      mystery: Mountain,
      default: Flower
    };
    const Icon = icons[name.toLowerCase()] || icons.default;
    return <Icon className="w-5 h-5" />;
  };

  const getHealthColor = (health) => {
    if (health >= 80) return 'text-green-600';
    if (health >= 60) return 'text-yellow-600';
    return 'text-red-600';
  };

  const getHealthBgColor = (health) => {
    if (health >= 80) return 'bg-green-100';
    if (health >= 60) return 'bg-yellow-100';
    return 'bg-red-100';
  };

  if (loading) {
    return (
      <div className="space-y-3">
        {[1, 2, 3].map(i => (
          <div key={i} className="animate-pulse">
            <div className="h-4 bg-gray-200 rounded w-1/2 mb-2"></div>
            <div className="h-2 bg-gray-200 rounded"></div>
          </div>
        ))}
      </div>
    );
  }

  if (!gardens.length) {
    return (
      <p className="text-gray-500 text-sm">No gardens found</p>
    );
  }

  return (
    <div className="space-y-3">
      {gardens.map(garden => (
        <div key={garden.id} className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <div className={`p-1 rounded ${getHealthBgColor(garden.health || 0)}`}>
              {getGardenIcon(garden.name)}
            </div>
            <div>
              <p className="text-sm font-medium text-gray-900 capitalize">
                {garden.name}
              </p>
              <p className="text-xs text-gray-500">
                {garden.memories || 0} memories
              </p>
            </div>
          </div>
          <div className="text-right">
            <p className={`text-sm font-medium ${getHealthColor(garden.health || 0)}`}>
              {garden.health || 0}%
            </p>
            <div className="w-16 bg-gray-200 rounded-full h-1.5 mt-1">
              <div
                className={`h-1.5 rounded-full ${
                  garden.health >= 80 ? 'bg-green-600' :
                  garden.health >= 60 ? 'bg-yellow-600' : 'bg-red-600'
                }`}
                style={{ width: `${garden.health || 0}%` }}
              ></div>
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};

export default GardenHealth;
