/*
module Home
( homeView
) where
*/


import * as Avers from '../lib/avers';
import Computation from '../lib/computation';
import {App} from '../app';

import {Card, tileHeader} from './Components/Card';
import {navBar} from './Components/NavBar';
import {sideBar} from './Components/SideBar';
import {site} from './Components/Site';

import {Boulder} from '../storage';


function threeBounceSpinner() {
    return React.DOM.div
        ( { className: 'sk-spinner sk-spinner-three-bounce' }
        , React.DOM.div({ className: 'sk-bounce1' })
        , React.DOM.div({ className: 'sk-bounce2' })
        , React.DOM.div({ className: 'sk-bounce3' })
        );
}

export function loadingTileBody() {
    return React.DOM.div
        ( { className: 'boulder-card-body' }
        , threeBounceSpinner()
        );
}

function boulderLoadingCard(app: App, boulderId) {
    return React.DOM.div
        ( { key: boulderId, className: 'boulder-card' }
        , tileHeader(app, boulderId)
        , loadingTileBody()
        );
}


export function
homeView(app: App) {
    var boulders = app.data.ownedBoulderCollection.ids.get([]).map(boulderId => {
        let boulderC = Avers.lookupEditable<Boulder>(app.data.aversH, boulderId);

        return boulderC.fmap((boulder) => {
             return Card({ app: app, key: boulder.objectId, boulderE: boulder });
        }).get(boulderLoadingCard(app, boulderId));
    });

    return site
        ( app
        , navBar(app)
        //, sideBar(app)
        , React.DOM.div({ className: 'boulders' }, boulders)
        );
}
