var listeners = [];

/**
 * called everytime new state is sent to the user. It will assign the new data
 * to the map using ids. It will also run all listener callbacks
 */
function trigger(fragment) {
  // maybe remove this
  fragment.syncedAt = new Date();

  listeners.forEach(function(listener) {
    listener.do(fragment);
  });
}

/**
 * @param {Object}   options for the listener
 * @param {Function} options.do to be performed
 *
 * @returns {Object} which represents a listener
 */
function listener(options) {
  var index = listeners.push(def);

  return Object.assign({
    index: index
  }, options);
}

/**
 * @param {Object} index
 * @returns {Boolean} true when the listener was removed
 */
function removeListener(listener) {
  return Boolean(listeners.slice(listener.index));
}

window.lasius = window.lasius || {};
window.lasius.sync = {
  trigger: trigger,
  listener: listener,
  removeListener: removeListener,
};

