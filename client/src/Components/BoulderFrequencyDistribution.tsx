import * as React from "react";
import styled from "styled-components";

import { useTypeface, copy14 } from "../Materials/Typefaces";
import { gradeBackgroundColor, gradeBorderColor } from "../Materials/Colors";

export interface BoulderFrequencyDistributionProps {
  data: Array<{ grade: string; count: number }>;
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
              background: gradeBackgroundColor(grade.toLowerCase()) + "26",
              borderWidth: (count > 0 ? 2 : 0),
              borderStyle: "solid",
              borderColor: gradeBorderColor(grade.toLowerCase()),
              borderBottom: "none"
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
  flex: 1;

  display: flex;
  align-items: flex-end;

  margin-bottom: 24px;
  border-bottom: 2px solid #666;

  & > div {
    flex: 1;
  }
`;

const Grade = styled.div`
  position: relative;
  margin-right: 4px;
  &:last-child {
    margin: 0;
  }
`;

const Percentage = styled.div`
  ${useTypeface(copy14)};
  color: #222222bb;
  text-align: center;
  position: absolute;
  left: 0;
  right: 0;
  bottom: 0;
  margin-bottom: -28px;
`;
