/*
module Home
( homeView
) where
*/

import * as React from 'react';
import strftime from 'strftime';

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

const BoulderCardLoading = ({ app, boulderId }: { app: App, boulderId: string }) => (
    <div key={boulderId} className={'boulder-card'}>
        {tileHeader(app, boulderId)}
        <LoadingTileBody />
    </div>
);


export function
homeView(app: App) {
    // Get a list of all Editable<Boulder>, sort them by date.
    // TODO: sort on the server.
    const editableBoulders = app.data.activeBouldersCollection.ids.get([])
        .map(boulderId => Avers.lookupEditable<Boulder>(app.data.aversH, boulderId).get(null))
        .filter(x => x !== null)
        .sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());

    // Go through the list, render each boulder with <BoulderCard> and insert headings in
    // between two cards when the day they were created at changes.
    const {boulders} = editableBoulders.reduce(({boulders, date}, boulder) => {
        const {objectId, createdAt} = boulder;

        if (date === null) {
            return {
                boulders: boulders.concat([<BoulderCard key={objectId} app={app} boulderE={boulder} />]),
                date: createdAt
            };
        } else if (date.getMonth() === createdAt.getMonth() && date.getDate() === createdAt.getDate()) {
            return {
                boulders: boulders.concat([<BoulderCard key={objectId} app={app} boulderE={boulder} />]),
                date: createdAt
            };
        } else {
            return {
                boulders: boulders.concat(
                    [ <div className="boulder-separator">{strftime('%A, %e. %B', createdAt)}</div>
                    , <BoulderCard key={objectId} app={app} boulderE={boulder} />
                    ]),
                date: createdAt
            };
        }
    }, { boulders: [], date: null });

    return (
        <Site app={app}>
            <div className="boulders">
                {boulders}
            </div>
        </Site>
    );
}
