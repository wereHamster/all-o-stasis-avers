/*
module Home
( homeView
) where
*/

import * as React from 'react';

import * as Avers from 'avers';
import Computation from 'computation';
import {App} from '../app';

import {BoulderCard, tileHeader} from './Components/BoulderCard';
import {Site} from './Components/Site';

import {Boulder} from '../storage';


function threeBounceSpinner() {
    return (
        <div className='sk-spinner sk-spinner-three-bounce'>
            <div className='sk-bounce1'></div>
            <div className='sk-bounce2'></div>
            <div className='sk-bounce3'></div>
        </div>
    );
}

export function loadingTileBody() {
    return (
        <div className='boulder-card-body'>
            {threeBounceSpinner()}
        </div>
    );
}

function boulderLoadingCard(app: App, boulderId) {
    return (
        <div key={boulderId} className={'boulder-card'}>
            {tileHeader(app, boulderId)}
            {loadingTileBody()}
        </div>
    );
}


export function
homeView(app: App) {
    var boulders = app.data.activeBouldersCollection.ids.get([]).map(boulderId => {
        let boulderC = Avers.lookupEditable<Boulder>(app.data.aversH, boulderId);

        return boulderC.fmap((boulder) => {
             return BoulderCard({ app: app, key: boulder.objectId, boulderE: boulder }) as JSX.Element;
        }).get(boulderLoadingCard(app, boulderId));
    });

    return (
        <Site app={app}>
            <div className="boulders">
                <ul>{boulders}</ul>
            </div>
        </Site>
    );
}
