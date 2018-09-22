import * as Avers from "avers";

import { App, Data, infoTable } from "../app";

// ----------------------------------------------------------------------------
// Create an Avers handle and App object

const fetch = url => Promise.reject(new Error(`Failed request to ${url}`));

export const aversH = new Avers.Handle({
  apiHost: "localhost",
  fetch,
  createWebSocket: path => new WebSocket("ws:localhost" + path),
  now: () => window.performance.now(),
  infoTable
});
const data = new Data(aversH);
export const app = new App(null as any, data, () => {
  throw new Error("mkViewFn");
});

export const anonymousApp = app;

export const setterApp = new App(
  null as any,
  (() => {
    const d = new Data(aversH);
    d.session.objId = "setter-1";
    return d;
  })(),
  () => {
    throw new Error("mkViewFn");
  }
);

export const adminApp = new App(
    null as any,
    (() => {
      const d = new Data(aversH);
      d.session.objId = "admin-1";
      return d;
    })(),
    () => {
      throw new Error("mkViewFn");
    }
  );

// ----------------------------------------------------------------------------
// Fill Avers handle with a few boulders and setters

export const boulder1Id = "boulder-1";
Avers.resolveEditable(aversH, boulder1Id, {
  id: boulder1Id,
  type: "boulder",
  createdAt: "2017-02-02T01:30:55Z",
  content: {
    setter: ["setter-1", "setter-2"],
    sector: "spektrumone",
    grade: "yellow",
    gradeNr: 1,
    removed: -1,
    name: "name"
  }
});

export const setter1Id = "setter-1";
Avers.resolveEditable(aversH, setter1Id, {
  id: setter1Id,
  type: "account",
  createdAt: "2017-02-02T01:30:55Z",
  content: {
    login: "wereHamster",
    role: "setter",
    email: "tomas.carnecky@gmail.com",
    name: "Tomas Carnecky"
  }
});

export const setter2Id = "setter-2";
Avers.resolveEditable(aversH, setter2Id, {
  id: setter2Id,
  type: "account",
  createdAt: "2017-02-02T01:30:55Z",
  content: {
    login: "iff",
    role: "setter",
    email: "iff@yvesineichen.com",
    name: "Yves Ineichen"
  }
});


export const admin1Id = "admin-1";
Avers.resolveEditable(aversH, admin1Id, {
  id: admin1Id,
  type: "account",
  createdAt: "2017-02-02T01:30:55Z",
  content: {
    login: "wereHamster",
    role: "admin",
    email: "tomas.carnecky@gmail.com",
    name: "Tomas Carnecky"
  }
});
