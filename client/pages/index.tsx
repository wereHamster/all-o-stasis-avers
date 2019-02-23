import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";
import { format } from "date-fns";

import { App } from "../src/app";
import { Boulder, boulderCompare } from "../src/storage";

import { text } from "../src/Materials/Colors";
import { useTypeface, copy16Bold } from "../src/Materials/Typefaces";

import { BoulderCard } from "../src/Views/Components/BoulderCard";
import { Site } from "../src/Views/Components/Site";
import { BoulderId24 } from "../src/Views/Components/BoulderId";
import { Input } from "../src/Components/Input";
import { Button } from "../src/Components/Button";
import { createBoulder, role } from "../src/actions";

interface State {
  search: string;
  grades: string[];
}

export default class extends React.Component<{ app: App }, State> {
  state: State = {
    search: "",
    grades: []
  };

  changeSearch = (ev): void => {
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

    const groups = [] as Array<{ date: Date; boulders: Array<Avers.Editable<Boulder>> }>;

    editableBoulders.forEach(boulder => {
      if (!boulder) {
        return;
      }

      const createdAt = new Date(boulder.content.setDate);
      const lastGroup = groups[groups.length - 1];

      if (lastGroup === undefined) {
        groups.push({ date: createdAt, boulders: [boulder] });
      } else if (
        lastGroup.date.getMonth() === createdAt.getMonth() &&
        lastGroup.date.getDate() === createdAt.getDate()
      ) {
        lastGroup.boulders.push(boulder);
      } else {
        lastGroup.boulders.sort((a, b) => boulderCompare(a.content, b.content));
        groups.push({ date: createdAt, boulders: [boulder] });
      }
    });

    return (
      <Site app={app}>
        <BoulderFilter>
          <div style={{ marginRight: 32 }}>
            <BoulderFilterHeader>Filter</BoulderFilterHeader>
            <div style={{ display: "flex" }}>
              <BoulderGradeToggleButton grade="yellow" grades={grades} onToggle={this.toggleGrade} />
              <BoulderGradeToggleButton grade="green" grades={grades} onToggle={this.toggleGrade} />
              <BoulderGradeToggleButton grade="orange" grades={grades} onToggle={this.toggleGrade} />
              <BoulderGradeToggleButton grade="blue" grades={grades} onToggle={this.toggleGrade} />
              <BoulderGradeToggleButton grade="red" grades={grades} onToggle={this.toggleGrade} />
              <BoulderGradeToggleButton grade="white" grades={grades} onToggle={this.toggleGrade} />
            </div>
          </div>

          <div style={{ marginRight: 32 }}>
            <BoulderFilterHeader>Search</BoulderFilterHeader>
            <div style={{ width: 200, maxWidth: 200 }}>
              <Input placeholder="Grade Nr." value={search} onChange={this.changeSearch} />
            </div>
          </div>

        {role(app) !== "user" && (
              <div style={{ marginRight: 32, display: 'block' }}>
                <BoulderFilterHeader>Actions</BoulderFilterHeader>
                <div>
                  <Button
                    onClick={e => {
                      e.stopPropagation();
                      createBoulder(app);
                    }}
                  >
                    Create new boulder
                  </Button>
                </div>
              </div>
            )}
        </BoulderFilter>

        <Boulders>
          {groups.map(({ date, boulders }) => (
            <React.Fragment key={date.toISOString()}>
              <BoulderSeparator key={`separator-${date.toISOString()}`}>{format(date, "dd. MMMM")}</BoulderSeparator>
              {boulders.map(boulder => (
                <BoulderCard key={boulder.objectId} app={app} boulderE={boulder} />
              ))}
            </React.Fragment>
          ))}
        </Boulders>
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
  display: none;
  align-items: flex-start;
  padding: 20px 16px 12px;
  @media (min-width: 600px) {
    display: flex;
    padding: 20px 24px 20px;
  }
`;
const BoulderFilterHeader = styled.div`
  ${useTypeface(copy16Bold)};
  color: ${text};
  padding-right: 20px;
`;

const BoulderGradeToggle = styled.div`
  cursor: pointer;
  margin: 8px 4px 8px 0;
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
