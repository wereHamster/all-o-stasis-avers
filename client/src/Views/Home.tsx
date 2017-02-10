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


const ThreeBounceSpinner = () => (
    <div className='sk-spinner sk-spinner-three-bounce'>
        <div className='sk-bounce1'></div>
        <div className='sk-bounce2'></div>
        <div className='sk-bounce3'></div>
    </div>
);

export const LoadingTileBody = () => (
    <div className='boulder-card-body'>
        <ThreeBounceSpinner />
    </div>
);

function boulderLoadingCard(app: App, boulderId) {
    return (
        <div key={boulderId} className={'boulder-card'}>
            {tileHeader(app, boulderId)}
            <LoadingTileBody />
        </div>
    );
}


export function
homeView(app: App) {
    var boulders = app.data.activeBouldersCollection.ids.get([]).map(boulderId => {
        const boulderC = Avers.lookupEditable<Boulder>(app.data.aversH, boulderId);

        return boulderC.fmap((boulder) =>
             <BoulderCard app={app} key={boulder.objectId} boulderE={boulder} />
        ).get(boulderLoadingCard(app, boulderId));
    });

    return (
        <Site app={app}>
            <div className="boulders">
                {boulders}
            </div>
        </Site>
    );
}
