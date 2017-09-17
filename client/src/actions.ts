/*
module Actions
( createBoulder
, role
) where
*/

import * as Avers from 'avers';
import * as Storage from './storage';
import {App, navigateTo, refresh} from './app';
import {Account} from './storage';

function mkBoulder(app: App) : Storage.Boulder {
    const now = Date.now();
    return Avers.mk(Storage.Boulder,
        { setter : [app.data.session.objId],
          setDate : now
        }
    );
}

export function
createBoulder(app: App) {
    let promise = Avers.createObject(app.data.aversH, 'boulder', mkBoulder(app)).then(id => {
        app.createBoulderPromise = undefined;
        Avers.resetObjectCollection(app.data.ownedBoulderCollection);
        navigateTo('/');
        return id;
    });

    app.createBoulderPromise = promise;
    refresh(app);

    return promise;
}

export function
role(app: App) : string {
    let accountC = Avers.lookupContent<Account>(app.data.aversH, app.data.session.objId);
    let accountE = accountC.get(null);

    if (accountE === null)
        return "user";
    else
        return accountE.role;
}

// filter on the client
export function
sectorBoulders(app: App, sectorName: String) : Avers.Editable<Storage.Boulder>[] {
    return app.data.activeBouldersCollection.ids.get([])
        .map(boulderId => Avers.lookupEditable<Storage.Boulder>(app.data.aversH, boulderId).get(null))
        .filter(x => x !== null)
        .filter(x => x.content.sector == sectorName);
}