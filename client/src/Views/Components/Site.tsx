import * as Avers from "avers";
import * as React from "react";

import { role } from "../../actions";
import { App } from "../../app";

import { AdminBar } from "../../Components/AdminBar";
import { NavBar } from "../../Components/NavBar";
import { TransientNotification } from "./TransientNotification";

interface SiteProps {
  app: App;
  children?: React.ReactNode;
}

export class Site extends React.Component<SiteProps> {
  onClick = () => {
    Avers.startNextGeneration(this.props.app.data.aversH);
  };

  render() {
    const { app, children } = this.props;

    return (
      <div style={{ minHeight: "100vh", display: "flex", flexDirection: "column" }} onClick={this.onClick}>
        {role(app) === "admin" && <AdminBar app={app} />}
        <NavBar app={app} />
        {children}
        <TransientNotification app={app} />
      </div>
    );
  }
}
