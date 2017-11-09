import * as Avers from 'avers'
import {App, navigateTo, refresh} from './app'
import * as Storage from './storage'

function mkBoulder(app: App): Storage.Boulder {
    const now = Date.now()
    return Avers.mk(Storage.Boulder,
        { setter : [app.data.session.objId],
          setDate : now,
        },
    )
}

export function
createBoulder(app: App) {
    const promise = Avers.createObject(app.data.aversH, 'boulder', mkBoulder(app)).then(id => {
        app.createBoulderPromise = undefined
        Avers.resetObjectCollection(app.data.ownedBoulderCollection)
        Avers.resetObjectCollection(app.data.activeBouldersCollection)
        refresh(app)
        navigateTo('/boulder/' + id)
        return id
    })

    app.createBoulderPromise = promise
    refresh(app)

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
    return app.data.activeBouldersCollection.ids.get([])
        .map(boulderId => Avers.lookupEditable<Storage.Boulder>(app.data.aversH, boulderId).get(null as any))
        .filter(x => x !== null)
        .filter(x => x.content.sector === sectorName)
}

export function
activeBoulders(app: App): Array<Avers.Editable<Storage.Boulder>> {
    return app.data.activeBouldersCollection.ids.get([])
        .map(boulderId => Avers.lookupEditable<Storage.Boulder>(app.data.aversH, boulderId).get(null as any))
        .filter(x => x !== null)
}

export function
removeBoulders(boulders: Array<Avers.Editable<Storage.Boulder>>) {
    // remove all boulders on the currently active sector
    if (window.confirm('Wirklich alle Boulder entfernen?')) {
        const now = Date.now()
        boulders.forEach(boulder => {
            boulder.content.removed = now.valueOf()
        })
    }
}
