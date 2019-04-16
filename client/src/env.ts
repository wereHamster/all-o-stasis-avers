import * as Avers from "avers";
import * as React from "react";
import { config, Data, App, infoTable } from "./app";

export interface Env {
  app: App;
}

export const Env = React.createContext<Env>({
  app: ((): App => {
    const aversH = Avers.newHandle({
      apiHost: (config.secure ? "https://" : "http://") + config.apiHost,
      fetch:
        typeof window !== "undefined"
          ? window.fetch.bind(window)
          : async () => {
              throw new Error("fetch");
            },
      createWebSocket: path => new WebSocket((config.secure ? "wss://" : "ws://") + config.apiHost + path),
      now: typeof window !== "undefined" ? window.performance.now.bind(window.performance) : () => 0,
      infoTable
    });

    const data = new Data(aversH);
    return new App(data);
  })()
});

export const useEnv = () => {
  return React.useContext(Env);
};
