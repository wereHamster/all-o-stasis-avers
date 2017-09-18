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

import * as React from 'react';
import * as ReactDOM from 'react-dom';

import * as Avers from 'avers';
import {Account, Boulder} from './storage';

import * as page from 'page'

import configObject from './config';

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

infoTable.set('account',  Account);
infoTable.set('boulder',  Boulder);

export class Data {

    session : Avers.Session;

    public accountsCollection     : Avers.ObjectCollection;
    public adminAccountCollection : Avers.ObjectCollection;

    public bouldersCollection       : Avers.ObjectCollection;
    public activeBouldersCollection : Avers.ObjectCollection;
    public ownedBoulderCollection   : Avers.ObjectCollection;

    constructor(public aversH: Avers.Handle) {

        this.session = new Avers.Session(aversH);

        // Collection of all accounts for user view
        // FIXME: where do we need the this collection??
        this.accountsCollection =
            new Avers.ObjectCollection(aversH, 'accounts');

        // Collection of admin/setter accounts (display team)
        this.adminAccountCollection =
            new Avers.ObjectCollection(aversH, 'adminAccounts');

        // Collecion of all boulders to compute statistics
        this.bouldersCollection =
            new Avers.ObjectCollection(aversH, 'boulders');

        // Collection of all boulders currently active in the gym
        this.activeBouldersCollection =
            new Avers.ObjectCollection(aversH, 'activeBoulders');

        // Collection of boulders for a given user (setter/admin)
        this.ownedBoulderCollection =
            new Avers.ObjectCollection(aversH, 'ownedBoulders');
    }
}


export function
refresh(app: App): void {
    requestAnimationFrame(() => {
        ReactDOM.render(app.mkViewFn(app), app.containerElement)
    })
}

export function
loadView(app: App, mkViewFn: (app: App) => React.ReactElement<any>): void {
    app.mkViewFn = mkViewFn;
    refresh(app);
}

export function
navigateTo(p : string) {
    page(p);
}

export function
navigateToFn(p: string) {
    return () => { navigateTo(p); }
}

