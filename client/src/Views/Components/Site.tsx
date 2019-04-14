import * as Avers from "avers";
import * as React from "react";

import { role } from "../../actions";
import { App } from "../../app";

import { AdminBar } from "../../Components/AdminBar";
import { Header } from "../../Components/Header/Header";
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
        <Header app={app} />
        {role(app) === "admin" && <AdminBar />}
        {children}
        <TransientNotification app={app} />
      </div>
    );
  }
}
