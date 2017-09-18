import * as Avers from 'avers'

import {App, Data, infoTable} from '../app'
import {Boulder} from '../storage'


// ----------------------------------------------------------------------------
// Create an Avers handle and App object

const fetch = (url) =>
    Promise.reject(new Error(`Failed request to ${url}`))

export const aversH = new Avers.Handle('localhost', fetch, path => new WebSocket('ws:localhost' + path), () => window.performance.now(), infoTable)
const data = new Data(aversH)
export const app = new App(null, data, () => { throw new Error('mkViewFn') })



// ----------------------------------------------------------------------------
// Fill Avers handle with a few boulders and setters

export const boulder1Id = 'boulder-1'
Avers.resolveEditable(aversH, boulder1Id,
    { id: boulder1Id
    , type: 'boulder'
    , createdAt: '2017-02-02T01:30:55Z'
    , content:
        { setter: ['setter-1', 'setter-2']
        , sector: 'spektrumone'
        , grade: 'yellow'
        , gradeNr: 1
        , removed: -1
        , name: 'name',
        },
    },
)

export const setter1Id = 'setter-1'
Avers.resolveEditable(aversH, setter1Id,
    { id: setter1Id
    , type: 'account'
    , createdAt: '2017-02-02T01:30:55Z'
    , content:
        { login: 'wereHamster'
        , role: 'admin'
        , email: 'tomas.carnecky@gmail.com'
        , name: 'Tomas Carnecky',
        },
    },
)

export const setter2Id = 'setter-2'
Avers.resolveEditable(aversH, setter2Id,
    { id: setter2Id
    , type: 'account'
    , createdAt: '2017-02-02T01:30:55Z'
    , content:
        { login: 'iff'
        , role: 'admin'
        , email: 'iff@yvesineichen.com'
        , name: 'Yves Ineichen',
        },
    },
)
