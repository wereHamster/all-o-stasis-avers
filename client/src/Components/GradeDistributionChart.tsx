import * as React from "react";
import styled from "styled-components";
import Measure, { BoundingRect } from "react-measure";

import { useTypeface, copy14 } from "../Materials/Typefaces";
import { gradeBackgroundColor, gradeBorderColor } from "../Materials/Colors";
import { scaleBand, scaleLinear } from "d3-scale";
import { grades } from "../storage";
import { GridLines, GridLabels } from "../Views/Components/Stats/Visualization";

export interface GradeDistributionChartProps {
  data: Array<{ grade: string; count: number }>;
}

export class GradeDistributionChart extends React.Component<GradeDistributionChartProps> {
  render() {
    const { data } = this.props;

    return (
      <Measure bounds>
        {({ measureRef, contentRect }) => (
          <div ref={measureRef} style={{ position: "relative", flex: 1 }}>
            {contentRect.bounds && <Chart bounds={contentRect.bounds} data={data} />}
          </div>
        )}
      </Measure>
    );
  }
}

interface ChartProps {
  bounds: BoundingRect;
  data: Array<{ grade: string; count: number }>;
}

const Chart = ({ bounds, data }: ChartProps) => {
  const padding = {
    top: 24,
    left: 24,
    right: 24,
    bottom: 48
  };

  const max = Math.max(3, ...data.map(x => x.count));

  const xScale = scaleBand()
    .domain(grades)
    .range([0, bounds.width - padding.left - padding.right - 24])
    .paddingOuter(0.2)
    .paddingInner(0.2);

  const yScale = scaleLinear()
    .domain([0, Math.round(max * 1.2)])
    .range([bounds.height - padding.bottom - padding.top, 0]);

  return (
    <svg width={bounds.width} height={bounds.height} style={{ position: "absolute", display: "block" }}>
      <g transform={`translate(${padding.left},${padding.top})`}>
        {data.map(({ grade, count }) => (
          <Rect
            key={grade}
            x={xScale(grade)}
            y={yScale(count)}
            width={xScale.bandwidth()}
            height={yScale(0) - yScale(count)}
            fill={gradeBackgroundColor(grade.toLowerCase())}
            stroke={gradeBorderColor(grade.toLowerCase())}
          />
        ))}

        <GridLines width={bounds.width - padding.left - padding.right} yScale={yScale} />
        <GridLabels width={bounds.width - padding.left - padding.right} yScale={yScale} />
      </g>
      <g transform={`translate(${padding.left},${bounds.height - padding.bottom})`}>
        {data.map(({ grade, count }) => (
          <Text key={grade} x={(xScale(grade) || 0) + xScale.bandwidth() / 2} y={20}>
            {count}
          </Text>
        ))}
      </g>
    </svg>
  );
};

const Rect = styled.rect`
  stroke-width: 2;
  fill-opacity: 0.4;
  transition: fill-opacity 0.36s;

  &:hover {
    fill-opacity: 1;
  }
`;

const Text = styled.text`
  ${useTypeface(copy14)};
  fill: #222222bb;
  text-anchor: middle;
`;
