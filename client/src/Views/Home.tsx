import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";
import { format } from "date-fns";

import { App } from "../app";
import { Boulder } from "../storage";

import { text } from "../Materials/Colors";
import { useTypeface, copy16Bold } from "../Materials/Typefaces";

import { BoulderCard } from "./Components/BoulderCard";
import { Site } from "./Components/Site";
import { BoulderId24 } from "./Components/BoulderId";
import { Input } from "../Components/Input";

interface State {
  search: string;
  grades: string[];
}

export default class extends React.Component<{ app: App }, State> {
  state: State = {
    search: "",
    grades: []
  };

  chaneSearch = (ev): void => {
    this.setState({ search: ev.target.value });
  };

  toggleGrade = (grade: string): void => {
    const { grades } = this.state;
    if (grades.indexOf(grade) === -1) {
      this.setState({ grades: [grade].concat(grades) });
    } else {
      this.setState({ grades: grades.filter(x => x !== grade) });
    }
  };

  render() {
    const { app } = this.props;
    const { search, grades } = this.state;

    const editableBoulders = app.data.activeBouldersCollection.ids
      .get<string[]>([])
      .map(boulderId => Avers.lookupEditable<Boulder>(app.data.aversH, boulderId).get(null))
      .filter(x => {
        if (x === null) {
          return false;
        } else if (search === "" && grades.length === 0) {
          return true;
        } else {
          return grades.indexOf(x.content.grade) !== -1 && (search === "" || +search === x.content.gradeNr);
        }
      });

    // Go through the list, render each boulder with <BoulderCard> and insert
    // headings in between two cards when the day they were created at changes.
    const res = editableBoulders.reduce(
      ({ boulders, date }, boulder) => {
        if (!boulder) {
          return { boulders, date };
        }

        const objectId = boulder.objectId;
        const createdAt = new Date(boulder.content.setDate);

        if (date === null) {
          return {
            boulders: boulders.concat([
              <BoulderSeparator key={`separator-${createdAt}`}>{format(createdAt, "dd. MMMM")}</BoulderSeparator>,
              <BoulderCard key={objectId} app={app} boulderE={boulder} />
            ]),
            date: createdAt
          };
        } else if (date.getMonth() === createdAt.getMonth() && date.getDate() === createdAt.getDate()) {
          return {
            boulders: boulders.concat([<BoulderCard key={objectId} app={app} boulderE={boulder} />]),
            date: createdAt
          };
        } else {
          return {
            boulders: boulders.concat([
              <BoulderSeparator key={`separator-${createdAt}`}>{format(createdAt, "dd. MMMM")}</BoulderSeparator>,
              <BoulderCard key={objectId} app={app} boulderE={boulder} />
            ]),
            date: createdAt
          };
        }
      },
      { boulders: [] as JSX.Element[], date: null as null | Date }
    );

    return (
      <Site app={app}>
        <BoulderFilter>
          <BoulderFilterHeader>Filters</BoulderFilterHeader>
          <div style={{ display: "flex", marginRight: 20 }}>
            <BoulderGradeToggleButton grade="yellow" grades={grades} onToggle={this.toggleGrade} />
            <BoulderGradeToggleButton grade="green" grades={grades} onToggle={this.toggleGrade} />
            <BoulderGradeToggleButton grade="orange" grades={grades} onToggle={this.toggleGrade} />
            <BoulderGradeToggleButton grade="blue" grades={grades} onToggle={this.toggleGrade} />
            <BoulderGradeToggleButton grade="red" grades={grades} onToggle={this.toggleGrade} />
            <BoulderGradeToggleButton grade="white" grades={grades} onToggle={this.toggleGrade} />
          </div>

          <div style={{ width: 200, maxWidth: 200 }}>
            <Input placeholder="Grade Nr." value={search} onChange={this.chaneSearch} />
          </div>
        </BoulderFilter>
        <Boulders>{res.boulders}</Boulders>
      </Site>
    );
  }
}

const BoulderGradeToggleButton = ({ grade, grades, onToggle }) => (
  <BoulderGradeToggle
    onClick={() => {
      onToggle(grade);
    }}
  >
    <BoulderId24 grade={grade}>{grades.indexOf(grade) === -1 ? "" : <Cross />}</BoulderId24>
  </BoulderGradeToggle>
);

// ----------------------------------------------------------------------------
const BoulderFilter = styled.div`
  display: flex;
  align-items: center;
  padding: 20px 16px 12px;
  @media (min-width: 600px) {
    padding: 20px 24px 20px;
  }
`;
const BoulderFilterHeader = styled.div`
  ${useTypeface(copy16Bold)};
  color: ${text};
  padding-right: 20px;
`;

const BoulderGradeToggle = styled.div`
  margin-right: 4px;
  cursor: pointer;
`;

const Boulders = styled.div`
  margin-top: 1rem;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
`;

const BoulderSeparator = styled.div`
  flex: 0 0 100%;
  width: 100%;
  padding: 20px 16px 12px;

  ${useTypeface(copy16Bold)};
  color: ${text};

  &:first-of-type {
    padding-top: 10px;
  }

  @media (min-width: 600px) {
    padding: 80px 24px 12px;
    &:first-of-type {
      padding: 20px 24px 12px;
    }
  }
`;

const Cross = () => (
  <svg width="18" height="18">
    <path stroke="currentColor" strokeWidth="2" d="M 2 2 L 16 16" />
    <path stroke="currentColor" strokeWidth="2" d="M 16 2 L 2 16" />
  </svg>
);
