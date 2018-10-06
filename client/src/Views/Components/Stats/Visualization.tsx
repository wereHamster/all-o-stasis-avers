import * as React from "react";
import Measure from "react-measure";
import { Motion, spring } from "react-motion";

import { scaleTime, scaleLinear, scaleOrdinal } from "d3-scale";
import { stack, area, line, curveLinear } from "d3-shape";
import { select } from "d3-selection";
import { axisBottom } from "d3-axis";

import { BoulderStat, grades } from "../../../storage";

import { yellow100, green100, orange100, blue100, red100 } from "../../../Materials/Colors";

const curve = curveLinear;

const matchSector = sectors =>
  sectors.length === 0 ? () => true : (bs: BoulderStat) => sectors.indexOf(bs.sector) !== -1;

const matchSetter = selectedSetters =>
  selectedSetters.length === 0
    ? () => true
    : (bs: BoulderStat) => bs.setters.some(setterId => selectedSetters.indexOf(setterId) !== -1);

export const Visualization = ({ bssC, sectors, selectedSetters }) => (
  <Measure bounds>
    {({ measureRef, contentRect }) => (
      <div ref={measureRef} style={{ flex: 1 }}>
        {contentRect.bounds && (
          <VisualizationRenderer
            bssC={bssC}
            bounds={contentRect.bounds}
            sectors={sectors}
            selectedSetters={selectedSetters}
          />
        )}
      </div>
    )}
  </Measure>
);

const VisualizationRenderer = ({ bssC, sectors, selectedSetters, bounds }) => {
  if (!bounds.height) {
    return <div />;
  }

  const events = bssC.get([]);

  const values = (() => {
    const res = events.reduce(
      (acc, ev) => {
        if (matchSector(sectors)(ev.bs) && matchSetter(selectedSetters)(ev.bs)) {
          if (ev.grade in acc.acc) {
            acc.acc[ev.grade] = Math.max(0, acc.acc[ev.grade] + (ev.type === "set" ? 1 : -1));
          } else if (ev.type === "set") {
            acc.acc[ev.grade] = 1;
          }
        }

        if (+acc.date === +ev.date) {
          return acc;
        } else {
          return {
            values: acc.values.concat([
              {
                date: acc.date,
                ...Object.keys(acc.acc).reduce((o, k) => ({ ...o, [k]: acc.acc[k] }), {})
              }
            ]),
            date: ev.date,
            acc: acc.acc
          };
        }
      },
      {
        values: [] as any,
        date: undefined as any,
        acc: grades().reduce((o, grade) => ({ ...o, [grade]: 0 }), {})
      }
    );

    return res.values
      .concat([
        {
          date: res.date,
          ...Object.keys(res.acc).reduce((o, k) => ({ ...o, [k]: res.acc[k] }), {})
        }
      ])
      .filter(x => x.date !== undefined);
  })();

  const xScale = scaleTime().range([0, bounds.width]);
  const yScale = scaleLinear().range([bounds.height - 40, 0]);
  const colorScale = scaleOrdinal([yellow100, green100, orange100, blue100, red100, "#FFFFFF"]);

  const skeys = grades();
  const s = stack()
    .keys(skeys)
    .value((d, key) => d[key] || 0);

  colorScale.domain(skeys);

  const areaGenerator = area()
    .curve(curve)
    .x(d => xScale(d.data.date))
    .y0(d => yScale(d[0]))
    .y1(d => yScale(d[1]));

  const data = s(values);

  xScale.domain([new Date(Date.now() - 4 * 30 * 24 * 60 * 60 * 1000), new Date(Date.now())]);
  yScale.domain([0, 300]);

  return (
    <svg width={bounds.width} height={bounds.height} style={{ display: "block" }}>
      <g transform={`translate(0,0)`}>
        {data.map((d, i) => (
          <Area key={i} index={i} colorScale={colorScale} data={d} a={areaGenerator} />
        ))}

        <TotalLine xScale={xScale} yScale={yScale} data={data} />

        <g
          transform={`translate(0,${bounds.height - 20})`}
          ref={el => {
            if (el) {
              select(el).call(axisBottom(xScale));
            }
          }}
        />
      </g>
    </svg>
  );
};

const Area = ({ index, colorScale, data, a }) => {
  if (data.length === 0) {
    return <g />;
  }

  const defaultStyle = {};
  data.forEach((d, i) => {
    defaultStyle[`v0_${i}`] = d[0];
    defaultStyle[`v1_${i}`] = d[1];
  });

  const style = {};
  data.forEach((d, i) => {
    style[`v0_${i}`] = spring(d[0]);
    style[`v1_${i}`] = spring(d[1]);
  });

  return (
    <Motion defaultStyle={defaultStyle} style={style}>
      {interpolatingStyle => {
        const dt = data.map((d, i) => {
          const x: any = [interpolatingStyle[`v0_${i}`], interpolatingStyle[`v1_${i}`]];
          x.data = d.data;
          return x;
        });

        return <path fill={colorScale(grades()[index])} d={a(dt)} />;
      }}
    </Motion>
  );
};

const TotalLine = ({ xScale, yScale, data }) => {
  const lineGenerator = line()
    .curve(curve)
    .x(d => xScale(d[0]))
    .y(d => yScale(d[1]));

  const total = data[data.length - 1];
  if (total.length === 0) {
    return <g />;
  }

  const defaultStyle = {};
  total.forEach((d, i) => {
    defaultStyle[`v${i}`] = d[1];
  });

  const style = {};
  total.forEach((d, i) => {
    style[`v${i}`] = spring(d[1]);
  });

  return (
    <Motion defaultStyle={defaultStyle} style={style}>
      {interpolatingStyle => {
        const dt = total.map((d, i) => [d.data.date, interpolatingStyle[`v${i}`]]);

        return <path fill="none" stroke="#222222" strokeWidth={2} d={lineGenerator(dt)} />;
      }}
    </Motion>
  );
};
