import * as Avers from 'avers';
import * as React from 'react';

import {markdown, ReactSpecimen} from 'catalog';

import {App, Data, infoTable} from '../../app';
import {Boulder} from '../../storage';

import {BoulderCard} from './BoulderCard';

// ----------------------------------------------------------------------------
// Setup of a fake Avers Handle, Data, and App instance.

const fetch = (url) =>
    Promise.reject(new Error("Failed request to " + url));

const aversH = new Avers.Handle('localhost', fetch, path => new WebSocket('ws:localhost' + path), () => window.performance.now(), infoTable);
const data = new Data(aversH);
const app = new App(null, data, () => { throw new Error('mkViewFn'); });


// Create one fake boulder inside the handle.
const boulderId = 'boulder-id';
Avers.resolveEditable(aversH, boulderId,
    { id: boulderId
    , type: 'boulder'
    , createdAt: '2017-02-02T01:30:55Z'
    , content:
        { setter: ['setter-1', 'setter-2']
        , sector: 'spektrumone'
        , grade: 'yellow'
        , gradeNr: 1
        , removed: -1
        , name: 'name'
        }
    }
);

Avers.resolveEditable(aversH, 'setter-1',
    { id: 'setter-1'
    , type: 'account'
    , createdAt: '2017-02-02T01:30:55Z'
    , content:
        { login: 'wereHamster'
        , role: 'admin'
        , email: 'tomas.carnecky@gmail.com'
        , name: 'Tomas Carnecky'
        }
    }
);
Avers.resolveEditable(aversH, 'setter-2',
    { id: 'setter-2'
    , type: 'account'
    , createdAt: '2017-02-02T01:30:55Z'
    , content:
        { login: 'iff'
        , role: 'admin'
        , email: 'iff@yvesineichen.com'
        , name: 'Yves Ineichen'
        }
    }
);

const boulderE = Avers.lookupEditable<Boulder>(aversH, boulderId).get(undefined);

export default () => markdown`
A yellow BoulderCard.

${
<ReactSpecimen noSource>
    <BoulderCard app={app} boulderE={boulderE} />
</ReactSpecimen>
}
`
