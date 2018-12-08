import * as Avers from "avers";
import { Account, Boulder } from "./storage";

import configObject from "./config";
import Computation from "computation";

export class Config {
  apiHost!: string;
  secure!: boolean;
  adminEmail!: string;
}

Avers.definePrimitive(Config, "apiHost", "localhost:8000");
Avers.definePrimitive(Config, "secure", false);
Avers.definePrimitive(Config, "adminEmail", "admin@boulder.app");

export const config = Avers.mk<Config>(Config, configObject);

export class App {
  createBoulderPromise: void | Promise<string> = undefined;
  constructor(public data: Data) {}
}

export const infoTable = new Map<string, { new (): any }>();

infoTable.set("account", Account);
infoTable.set("boulder", Boulder);

export class Data {
  session: Avers.Session;

  public accountsCollection: Avers.ObjectCollection;
  public adminAccountCollection: Avers.ObjectCollection;

  public bouldersCollection: Avers.ObjectCollection;
  public activeBouldersCollection: Avers.ObjectCollection;
  public ownedBoulderCollection: Avers.ObjectCollection;

  constructor(public aversH: Avers.Handle) {
    this.session = new Avers.Session(aversH);

    // Collection of all accounts for user view
    // FIXME: where do we need the this collection??
    this.accountsCollection = new Avers.ObjectCollection(aversH, "accounts");

    // Collection of admin/setter accounts (display team)
    this.adminAccountCollection = new Avers.ObjectCollection(aversH, "adminAccounts");

    // Collecion of all boulders to compute statistics
    this.bouldersCollection = new Avers.ObjectCollection(aversH, "boulders");

    // Collection of all boulders currently active in the gym
    this.activeBouldersCollection = new Avers.ObjectCollection(aversH, "activeBoulders");

    // Collection of boulders for a given user (setter/admin)
    this.ownedBoulderCollection = new Avers.ObjectCollection(aversH, "ownedBoulders");
  }
}

export const activeSetters = (app: App): Computation<string[]> =>
  Computation.liftA2(
    app.data.activeBouldersCollection.ids,
    app.data.adminAccountCollection.ids,
    (activeBoulderIds, adminAccountsIds) => {
      const accounts: string[] = [];

      const activeSetterIds = new Set<string>();
      activeBoulderIds.forEach(boulderId => {
        Avers.lookupContent<Boulder>(app.data.aversH, boulderId)
          .fmap(boulder => {
            boulder.setter.forEach(setterId => {
              activeSetterIds.add(setterId);
            });
          })
          .get(undefined);
      });

      adminAccountsIds.forEach(accountId => {
        if (activeSetterIds.has(accountId)) {
          accounts.push(accountId);
        }
      });

      return accounts;
    }
  );
