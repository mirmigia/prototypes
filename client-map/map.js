function create(options) {
  var map = {
    svg: d3.select(options.container).append('svg'),
    entities: {},
    view: {
      zoom: 1,
      top: 0,
      left: 0,
      width: 500,
      height: 500,

      //the game iwdth and height that is visible
      dimensions: {
        width: undefined,
        height: undefined
      }
    },
  };

  lasius.sync.listener({
    description: 'when an entity appears/moves',

    when: function(fragment) {
      var entity = entities[fragment.id];

      return (!entity || entity.x !== fragment.x || entity.y !== fragment.y);
    },

    do: function(fragment) {
      var entity = entities[fragment.id];

      if (entity === undefined) {
        entities[fragment.id] = fragment;
      } else {
        Object.assign(
          entity,
          fragment,
        );
      }

      update(map);
    }
  });

  return map;
}

function update(map) {
  map.view.dimensions.width  = map.view.width * view.zoom;
  map.view.dimensions.height = map.view.height * view.zoom;
  map.view.top               = map.view.top;
  map.view.left              = map.view.left;
  map.view.right             = map.view.left + view.dimensions.width;
  map.view.bottom            = map.view.right + view.dimensions.height;
  map.view.width             = map.view.width;
  map.view.height            = map.view.height;

  var data = d3.values(map.entities).filter(function(entity) {
    return isVisible(map.view, entity.value);
  });

  let entityGroups = map.svg.selectAll('circle')
    .data(data, prop('key'));

  updateEntityGroups.call(entityGroups, map);
  entityGroups.create().append('circle').call(updateEntityGroups, map);
  entityGroups.exit().remove();

  return map;
}

/**
 * onlt display entities that have a position
 *
 * @param {Object} entity
 */
function isVisible(view, entity) {
  return (entity.x && entity.y) &&
    within(view.left, view.right, entity.x) &&
    within(view.top, view.bottom, entity.y);
}

function remove(map) {
  // never remove
}

function updateEntityGroups(groups, map) {
  let pos = zoom(map.view, entity)

  return groups.attr({
    cx: pos.x,
    cy: pos.y,
    r: 20,
    fill: 'red',
  });
}

function within(left, right, value) {
  return (value => left && value <= right);
}

/**
 * calculates the location on view based on zoom.
 *
 * @param {Object} view containing data about the viewport
 * @param {Object} position containing an x and y
 *
 * @returns {Object} with an x and y value
 */
function zoom(view, position) {
  return {
    x: ((position.x - view.left) / view.dimensions.width)  * view.width;
    y: ((position.y - view.top)  / view.dimensions.height) * view.height;
  };
}

window.lasius = window.lasius || {};
window.lasius.map = {
  create: create,
  update: update,
  remove: remove,
};
