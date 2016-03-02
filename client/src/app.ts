/// <reference path="./ext/react.d.ts" />
/// <reference path="./ext/react-dom.d.ts" />

/*
module App
( App
, Data
, refresh
, loadView
, navigateTo
, navigateToFn
) where
*/


import * as Avers from 'avers';
import {Account, Activity, Boulder} from './storage';

import configObject from './config';

declare var page;

export class Config {
    apiHost : string;
}

Avers.definePrimitive(Config, 'apiHost', '//localhost:8000');

export const config = Avers.mk<Config>(Config, configObject);

export class App {

    createBoulderPromise : Promise<string> = undefined;

    constructor
      ( public containerElement : Element
      , public data             : Data
      , public mkViewFn         : (app: App) => any
      ) {}
}


export const infoTable = new Map<string, Avers.ObjectConstructor<any>>();
infoTable.set('account', Account);
infoTable.set('boulder', Boulder);


export class Data {

    session : Avers.Session;

    // Add collections, statics, ephemerals and other data which is managed
    // by avers here.
    //
    // clientVersion : Avers.Ephemeral<string>;
    // accountsCollection : Avers.ObjectCollection;

    public accountsCollection : Avers.ObjectCollection;
    public bouldersCollection : Avers.ObjectCollection;

    public adminAccountCollection : Avers.ObjectCollection;
    public activeBouldersCollection : Avers.ObjectCollection;
    public ownedBoulderCollection : Avers.ObjectCollection;

    public role : string;

    constructor(public aversH: Avers.Handle) {
        this.session = new Avers.Session(aversH);

        this.role = 'setter';

        this.accountsCollection =
            new Avers.ObjectCollection(aversH, 'accounts');
        this.bouldersCollection =
            new Avers.ObjectCollection(aversH, 'boulders');

        this.adminAccountCollection =
            new Avers.ObjectCollection(aversH, 'adminAccounts');
        this.activeBouldersCollection =
            new Avers.ObjectCollection(aversH, 'activeBoulders');
        this.ownedBoulderCollection =
            new Avers.ObjectCollection(aversH, 'ownedBoulders');
    }
}


export function
refresh(app: App): void {
    ReactDOM.render(app.mkViewFn(app), app.containerElement);
}

export function
loadView(app: App, mkViewFn: (app: App) => __React.ReactElement<any>): void {
    app.mkViewFn = mkViewFn;
    refresh(app);
}

export function
navigateTo(p : String) {
    page(p);
}

export function
navigateToFn(p: String) {
    return () => { navigateTo(p); }
}
