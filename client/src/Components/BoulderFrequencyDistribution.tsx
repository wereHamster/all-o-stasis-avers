import * as React from "react";
import styled from "styled-components";

import { useTypeface, copy14 } from "../Materials/Typefaces";
import { gradeBackgroundColor } from "../Materials/Colors";

export interface BoulderFrequencyDistributionProps {
  data: Array<{ grade: string, count: number }>;
}

export class BoulderFrequencyDistribution extends React.Component<BoulderFrequencyDistributionProps> {
  render() {
    const { data } = this.props;
    const max = Math.max(...data.map(x => x.count));

    return (
      <Root>
        {data.map(({ grade, count }) => (
          <Grade
            key={grade}
            style={{
              height: (40 * count) / max,
              background: gradeBackgroundColor(grade.toLowerCase())
            }}
          >
            <Percentage>{count}</Percentage>
          </Grade>
        ))}
      </Root>
    );
  }
}

const Root = styled.div`
  display: flex;
  align-items: flex-end;
  height: 60px;
  margin-bottom: 20px;
  padding-bottom: 1px;

  box-shadow: 0 1px 0 0 rgba(0, 0, 0, 0.2);

  & > div {
    flex: 1;
  }
`;

const Grade = styled.div`
  position: relative;
  margin: 0 2px;
`;

const Percentage = styled.div`
  ${useTypeface(copy14)};
  color: #222222bb;
  text-align: center;
  position: absolute;
  left: 0;
  right: 0;
  bottom: 0;
  margin-bottom: -22px;
`;
