import * as Avers from 'avers'
import {App} from './app'
import * as Storage from './storage'
import Router from 'next/router'

function mkBoulder(app: App): Storage.Boulder {
    const now = Date.now()
    return Avers.mk(Storage.Boulder,
        { setter : [app.data.session.objId],
          setDate : now,
        },
    )
}

export const resetBoulderCollections = (app: App): void => {
    Avers.resetObjectCollection(app.data.bouldersCollection)
    Avers.resetObjectCollection(app.data.ownedBoulderCollection)
    Avers.resetObjectCollection(app.data.activeBouldersCollection)
}

export function
createBoulder(app: App) {
    const promise = Avers.createObject(app.data.aversH, 'boulder', mkBoulder(app)).then(id => {
        resetBoulderCollections(app);
        Router.push({ pathname: '/boulder', query: { id }});
        return id
    })

    Avers.startNextGeneration(app.data.aversH);

    return promise
}

export function
role(app: App): string {
    const objId = app.data.session.objId
    if (!objId) {
        return 'user'
    }

    return Avers.lookupContent<Storage.Account>(app.data.aversH, objId)
        .fmap(accountE => accountE.role)
        .get('user')
}

// filter on the client
export function
sectorBoulders(app: App, sectorName: string): Array<Avers.Editable<Storage.Boulder>> {
    return app.data.activeBouldersCollection.ids.get<string[]>([])
        .map(boulderId => Avers.lookupEditable<Storage.Boulder>(app.data.aversH, boulderId).get(null))
        .filter(x => x !== null && x.content.sector === sectorName && !x.content.removed) as any
}

export function
activeBoulders(app: App): Array<Avers.Editable<Storage.Boulder>> {
    return app.data.activeBouldersCollection.ids.get<string[]>([])
        .map(boulderId => Avers.lookupEditable<Storage.Boulder>(app.data.aversH, boulderId).get(null))
        .filter(x => x !== null && !x.content.removed) as any
}

export function
removeBoulders(app: App, boulders: Array<Avers.Editable<Storage.Boulder>>) {
    // remove all boulders on the currently active sector
    if (window.confirm('Wirklich alle (' + boulders.length + ') Boulder entfernen?')) {
        const now = Date.now()
        boulders.forEach(boulder => {
            boulder.content.removed = now.valueOf()
        })

        resetBoulderCollections(app);
    }
}
