import * as React from "react";
import { markdown, ReactSpecimen } from "@catalog/core";
import { GradeDistributionChart } from "./GradeDistributionChart";

export default () => markdown`
${(
  <ReactSpecimen noSource span={3}>
    <div style={{ display: "flex", height: 250 }}>
      <GradeDistributionChart
        data={[
          { grade: "yellow", count: 12 },
          { grade: "green", count: 8 },
          { grade: "orange", count: 22 },
          { grade: "blue", count: 17 },
          { grade: "red", count: 15 },
          { grade: "white", count: 19 }
        ]}
      />
    </div>
  </ReactSpecimen>
)}
`;
