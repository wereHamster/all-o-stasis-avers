import * as React from "react";
import { Catalog, pageLoader, Theme } from "@catalog/core";

import * as C from "../src/Materials/Colors";

const theme: Partial<Theme> = {
  brandColor: C.darkSecondary,

  pageHeadingBackground: C.darkPrimary,
  pageHeadingTextColor: C.white,

  sidebarColorText: C.primaryText,
  sidebarColorTextActive: C.darkPrimary
};

const pages = [
  {
    path: "/",
    title: "Welcome",
    component: pageLoader(() => import("../README.md"))
  },
  {
    path: "/materials",
    title: "Materials",
    pages: [
      {
        path: "/materials/colors",
        title: "Colors",
        component: pageLoader(() => import("../src/Materials/Colors.doc"))
      },
      {
        path: "/materials/typfaces",
        title: "Typefaces",
        component: pageLoader(() => import("../src/Materials/Typefaces.doc"))
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
        component: pageLoader(() => import("../src/Components/Button.doc"))
      },
      {
        path: "/components/input",
        title: "Input",
        component: pageLoader(() => import("../src/Components/Input.doc"))
      },
      {
        path: "/components/navbar",
        title: "NavBar",
        component: pageLoader(() => import("../src/Components/NavBar.doc"))
      },
      {
        path: "/components/boulder-id",
        title: "BoulderId",
        component: pageLoader(() => import("../src/Views/Components/BoulderId.doc"))
      },
      {
        path: "/components/boulder-card",
        title: "BoulderCard",
        component: pageLoader(() => import("../src/Views/Components/BoulderCard.doc"))
      },
      {
        path: "/components/boulder-details",
        title: "BoulderDetails",
        component: pageLoader(() => import("../src/Views/Components/BoulderDetails.doc"))
      },
      {
        path: "/components/setter-card",
        title: "SetterCard",
        component: pageLoader(() => import("../src/Views/Components/SetterCard.doc"))
      },
      {
        path: "/components/sector-picker",
        title: "SectorPicker",
        component: pageLoader(() => import("../src/Views/Components/SectorPicker.doc"))
      },
      {
        path: "/components/login",
        title: "Login",
        component: pageLoader(() => import("../src/Views/Login.doc"))
      },
      {
        path: "/components/stats",
        title: "Stats",
        component: pageLoader(() => import("../src/Views/Components/Stats/Visualization.doc"))
      },
      {
        path: "/components/grade-distribution-chart",
        title: "GradeDistributionChart",
        component: pageLoader(() => import("../src/Components/GradeDistributionChart.doc"))
      }
    ]
  }
];

export default class extends React.PureComponent {
  state = {
    isMounted: false
  };

  componentDidMount() {
    this.setState({ isMounted: true });
  }

  render() {
    if (this.state.isMounted) {
      return <Catalog title="all-o-stasis" theme={theme} pages={pages} />;
    } else {
      return null;
    }
  }
}
