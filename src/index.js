import { pathOr } from 'ramda'

require('./styles.css')
require('tachyons')

{
  const [app, subscribe] = initElmModuleWithPortHelpers(
    {
      node: document.getElementById('root'),
      flags: {
        now: Date.now(),
        viewSize: [window.innerWidth, window.innerHeight],
        scrollbarSize: [
          window.innerWidth - document.body.clientWidth,
          window.innerHeight - document.body.clientHeight,
        ],
        oz: JSON.parse(localStorage.getItem('oz')) || null,
        logDict: JSON.parse(localStorage.getItem('logDict') || '{}'),
        projectDict: JSON.parse(
          localStorage.getItem('projectDict') || '{}',
        ),
        changes: JSON.parse(localStorage.getItem('changes') || null),
      },
    },
    require('./Main.elm'),
  )

  subscribe('getBeacons', function() {
    const beaconEls = [...document.querySelectorAll('[data-beacon]')]
    const beacons = beaconEls.map(beaconData)
    app.ports.gotBeacons.send(beacons)
  })

  subscribe('saveOZ', function(oz) {
    localStorage.setItem("oz", JSON.stringify(oz))
  })

  function beaconData(elem) {
    const boundingRect = elem.getBoundingClientRect()
    const beaconId = elem.getAttribute('data-beacon')
    return {
      id: tryParse(beaconId),
      x: boundingRect.x,
      y: boundingRect.y,
      width: boundingRect.width,
      height: boundingRect.height,
    }
  }

  function tryParse(str) {
    try {
      return JSON.parse(str)
    } catch (e) {
      return str
    }
  }
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
