import { zip, anyPass, pathOr, whereEq } from 'ramda'

require('./styles.css')
require('tachyons')

function parseTruthyOrNull(str) {
  try {
    return JSON.parse(str) || null
  } catch (e) {
    return null
  }
}

function fromKeys(kfn) {
  return fromKeysC1

  function fromKeysC1(ks) {
    return zip(ks, ks.map(kfn))
  }
}

function Cache(keys) {
  return {
    onCacheKV: ([key, val]) => {
      if (keys.includes(key)) {
        localStorage.setItem(key, JSON.stringify(val))
      } else {
        console.error('Invalid Cache Key:', key, 'validKeys:', keys)
      }
    },
    getAll: () => {
      const getParsed = key => parseTruthyOrNull(localStorage.getItem(key))
      return fromKeys(getParsed)(keys)
    },
  }
}

{
  const cache = Cache(['od', 'oz'])
  const [_, subscribe] = initElmModuleWithPortHelpers(
    {
      node: document.getElementById('root'),
      flags: {
        now: Date.now(),
        viewSize: [window.innerWidth, window.innerHeight],
        scrollbarSize: [
          window.innerWidth - document.body.clientWidth,
          window.innerHeight - document.body.clientHeight,
        ],
        ...cache.getAll(),
        oz: parseTruthyOrNull(localStorage.getItem('oz')),
        od: parseTruthyOrNull(localStorage.getItem('od')),
      },
    },

    require('./Main.elm'),
  )

  const [ctrl] = (function() {
    const noModifiers = {
      ctrlKey: false,
      shiftKey: false,
      altKey: false,
      metaKey: false,
    }

    const ctrl = key => whereEq({ ...noModifiers, ctrlKey: true, key })
    return [ctrl]
  })()

  window.addEventListener('keydown', function(e) {
    const shouldPreventDefault = e => anyPass([ctrl('o'), ctrl('s')])(e)

    if (shouldPreventDefault(e)) {
      e.preventDefault()
    }
  })

  subscribe('cacheKV', cache.onCacheKV)

  // subscribe('getBeacons', function() {
  //   const beaconEls = [...document.querySelectorAll('[data-beacon]')]
  //   const beacons = beaconEls.map(beaconData)
  //   app.ports.gotBeacons.send(beacons)
  // })
  //
  // function beaconData(elem) {
  //   const boundingRect = elem.getBoundingClientRect()
  //   const beaconId = elem.getAttribute('data-beacon')
  //   return {
  //     id: tryParse(beaconId),
  //     x: boundingRect.x,
  //     y: boundingRect.y,
  //     width: boundingRect.width,
  //     height: boundingRect.height,
  //   }
  // }

  // function tryParse(str) {
  //   try {
  //     return JSON.parse(str)
  //   } catch (e) {
  //     return str
  //   }
  // }
  // subscribe('focusSelector', function(selector) {
  //   requestAnimationFrame(function() {
  //     const el = document.querySelector(selector)
  //     if (el) {
  //       el.focus()
  //     } else {
  //       console.error('Focus Error ', selector)
  //     }
  //   })
  // })

  // subscribe('cacheLogDict', function(logDict) {
  //   localStorage.setItem('logDict', JSON.stringify(logDict))
  // })
  //
  // subscribe('cacheProjectDict', function(projectDict) {
  //   localStorage.setItem('projectDict', JSON.stringify(projectDict))
  // })
  //
  // subscribe('cacheChanges', function(changes) {
  //   localStorage.setItem('changes', JSON.stringify(changes))
  // })
}

function initElmModule(initParams, module) {
  const elmObject = module['Elm']
  const mainModuleName = Object.getOwnPropertyNames(elmObject)[0]
  const initFn = elmObject[mainModuleName]['init']
  return initFn(initParams)
}

function initElmModuleWithPortHelpers(initParams, module) {
  const app = initElmModule(initParams, module)
  function subscribe(portName, callback) {
    pathOr(() => console.error(`${portName}.subscribe Port Not Found`), [
      'ports',
      portName,
      'subscribe',
    ])(app)(callback)
  }

  return [app, subscribe]
}
