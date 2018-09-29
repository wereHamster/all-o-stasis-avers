import * as React from "react";
import { Catalog, pageLoader, Theme } from "@catalog/core";

import * as C from "../Materials/Colors";

const theme: Partial<Theme> = {
  brandColor: C.darkSecondary,
  pageHeadingBackground: C.darkPrimary,
  pageHeadingTextColor: C.primaryText
};

const pages = [
  {
    path: "/",
    title: "Welcome",
    component: pageLoader(() => import("../../README.md"))
  },
  {
    path: "/materials",
    title: "Materials",
    pages: [
      {
        path: "/materials/colors",
        title: "Colors",
        component: pageLoader(() => import("../Materials/Colors.doc"))
      },
      {
        path: "/materials/typfaces",
        title: "Typefaces",
        component: pageLoader(() => import("../Materials/Typefaces.doc"))
      }
    ]
  },
  {
    path: "/components",
    title: "Components",
    pages: [
      {
        path: "/components/button",
        title: "Button",
        component: pageLoader(() => import("../Components/Button.doc"))
      },
      {
        path: "/components/input",
        title: "Input",
        component: pageLoader(() => import("../Components/Input.doc"))
      },
      {
        path: "/components/navbar",
        title: "NavBar",
        component: pageLoader(() => import("../Components/NavBar.doc"))
      },
      {
        path: "/components/boulder-id",
        title: "BoulderId",
        component: pageLoader(() => import("./Components/BoulderId.doc"))
      },
      {
        path: "/components/boulder-card",
        title: "BoulderCard",
        component: pageLoader(() => import("./Components/BoulderCard.doc"))
      },
      {
        path: "/components/boulder-details",
        title: "BoulderDetails",
        component: pageLoader(() => import("./Components/BoulderDetails.doc"))
      },
      {
        path: "/components/setter-card",
        title: "SetterCard",
        component: pageLoader(() => import("./Components/SetterCard.doc"))
      },
      {
        path: "/components/sector-picker",
        title: "SectorPicker",
        component: pageLoader(() => import("./Components/SectorPicker.doc"))
      },
      {
        path: "/components/login",
        title: "Login",
        component: pageLoader(() => import("./Login.doc"))
      },
      {
        path: "/components/stats",
        title: "Stats",
        component: pageLoader(() => import("./Components/Stats/Visualization.doc"))
      }
    ]
  }
];

export default () => (
  <Catalog useBrowserHistory basePath="/_catalog" title="all-o-stasis" theme={theme} pages={pages} />
);
