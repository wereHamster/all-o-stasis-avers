import * as Avers from "avers";
import DayPicker from "react-day-picker";
import * as React from "react";
import styled from "styled-components";

import { role, resetBoulderCollections } from "../actions";
import { App } from "../app";
import { Account, Boulder } from "../storage";

import { text, darkGrey, primary, secondary, darkSecondary } from "../Materials/Colors";
import { useTypeface, copy16Bold, copy14 } from "../Materials/Typefaces";

import { NumberInput } from "./Components/NumberInput";
import { Site } from "./Components/Site";
import { BoulderDetails } from "./Components/BoulderDetails";
import { BoulderId } from "./Components/BoulderId";
import { SectorPicker } from "./Components/SectorPicker";
import { Button } from "../Components/Button";
import { BoulderSetterCard } from "./Components/BoulderSetterCard";

function BoulderDetailsEditor({ app, boulderE }: { app: App; boulderE: Avers.Editable<Boulder> }) {
  const boulder = boulderE.content;

  function changeSetDate(date) {
    boulder.setDate = date.valueOf();
    resetBoulderCollections(app);
  }

  function getSetDate(): Date {
    const initialDate = new Date(boulder.setDate);
    return initialDate;
  }

  function changeRemovedDate(date) {
    boulder.removed = date.valueOf();
    resetBoulderCollections(app);
  }

  function setRemoved() {
    changeRemovedDate(Date.now());
  }

  function clearRemoved() {
    changeRemovedDate(0);
  }

  function addSetter(accountId) {
    boulder.setter = [...boulder.setter, accountId];
  }

  function removeSetter(accountId) {
    if (confirm(`Are you sure you want to remove the setter?`)) {
      boulder.setter = boulder.setter.filter(x => x !== accountId);
    }
  }

  return (
    <div>
      <Section>Sector</Section>
      <div>
        <div style={{ maxWidth: 400 }}>
          <SectorPicker
            sectors={[boulder.sector]}
            onChange={sector => {
              boulder.sector = sector;
            }}
          />
        </div>
      </div>

      <Section>Setters</Section>
      <div>
        {boulder.setter.map((setterId, index) => (
          <BoulderSetterCard key={index} app={app} setterId={setterId} onClick={removeSetter} />
        ))}
        <AddSetter app={app} addSetter={addSetter} />
      </div>

      <Section>Grade</Section>
      <div>
        <div style={{ display: "flex", justifyContent: "space-between" }}>
          <GradeSelect boulder={boulder} grade="yellow" />
          <GradeSelect boulder={boulder} grade="green" />
          <GradeSelect boulder={boulder} grade="orange" />
          <GradeSelect boulder={boulder} grade="blue" />
          <GradeSelect boulder={boulder} grade="red" />
          <GradeSelect boulder={boulder} grade="white" />
        </div>
        <div style={{ marginTop: 12 }}>
          <NumberInput object={boulder} field="gradeNr" />
        </div>
      </div>

      <Section>Set Date</Section>
      <DayPickerWrapper>
        <DayPicker fixedWeeks initialMonth={getSetDate()} selectedDays={[getSetDate()]} onDayClick={changeSetDate} />
      </DayPickerWrapper>

      <Section>Danger Zone</Section>
      {(() => {
        if (boulder.removed > 0) {
          return (
            <div>
              <SectionLabel>The boulder was removed on</SectionLabel>
              <div>
                <DayPickerWrapper>
                  <DayPicker
                    fixedWeeks
                    initialMonth={new Date(boulder.removed)}
                    selectedDays={[new Date(boulder.removed)]}
                    onDayClick={changeRemovedDate}
                  />
                </DayPickerWrapper>
                <DangerButton onClick={clearRemoved}>Put back on wall</DangerButton>
              </div>
            </div>
          );
        } else {
          return (
            <div>
              <DangerButton onClick={setRemoved}>Remove</DangerButton>
            </div>
          );
        }
      })()}
    </div>
  );
}

export default (boulderId: string) => ({ app }: { app: App }) => {
  return Avers.lookupEditable<Boulder>(app.data.aversH, boulderId)
    .fmap(boulderE => {
      const boulderRep =
        role(app) === "user" ? (
          <BoulderDetails app={app} boulder={boulderE.content} />
        ) : (
          <BoulderDetailsEditor app={app} boulderE={boulderE} />
        );

      return (
        <Site app={app}>
          <Header app={app} boulder={boulderE.content} />
          <div style={{ margin: 24, display: "flex" }}>
            <div style={{ width: "400px", flex: "0 1 400px" }}>{boulderRep}</div>
          </div>
        </Site>
      );
    })
    .get(
      <Site app={app}>
        <div>Loading…</div>
      </Site>
    );
};

class Header extends React.Component<{ app: App; boulder: Boulder }> {
  render() {
    const { boulder } = this.props;

    return (
      <div style={{ margin: 24, display: "flex" }}>
        <BoulderId grade={boulder.grade}>{boulder.gradeNr}</BoulderId>
      </div>
    );
  }
}

// ----------------------------------------------------------------------------

const GradeSelect = ({ boulder, grade }: any) => (
  <GradeSelectButton
    onClick={() => {
      boulder.grade = grade;
    }}
  >
    <BoulderId grade={grade}>{boulder.grade === grade ? <Cross /> : ""}</BoulderId>
  </GradeSelectButton>
);

const GradeSelectButton = styled.div`
  cursor: pointer;

  & ${BoulderId} {
    transition: transform 0.2s;
  }

  &:hover ${BoulderId} {
    transform: scale(1.08);
  }
`;

const Cross = () => (
  <svg width="24" height="24">
    <path stroke="currentColor" strokeWidth="2" d="M 2 2 L 22 22" />
    <path stroke="currentColor" strokeWidth="2" d="M 22 2 L 2 22" />
  </svg>
);

// ----------------------------------------------------------------------------

class AddSetter extends React.Component<any, any> {
  state = {
    isOpen: false
  };

  open = ev => {
    ev.preventDefault();
    this.setState({ isOpen: true });
  };
  close = () => {
    this.setState({ isOpen: false });
  };
  addSetter = accountId => {
    this.props.addSetter(accountId);
    this.close();
  };

  render() {
    return (
      <AddSetterContainer>
        <a href="#" onClick={this.open}>
          Add setter
        </a>
        {this.state.isOpen && <SetterPicker app={this.props.app} dismiss={this.close} addSetter={this.addSetter} />}
      </AddSetterContainer>
    );
  }
}

const AddSetterContainer = styled.div`
  ${useTypeface(copy14)};
  cursor: pointer;

  & a {
    color: ${secondary};
    text-decoration: none;
    transition: all 0.16s;
  }

  & a:hover {
    color: ${darkSecondary};
    text-decoration: underline;
  }
`;

// ----------------------------------------------------------------------------

class SetterPicker extends React.Component<any> {
  onKeyPress = ev => {
    if (ev.key === "Escape") {
      this.props.dismiss();
    }
  };

  componentDidMount() {
    window.addEventListener("keyup", this.onKeyPress);
  }
  componentWillUnmount() {
    window.removeEventListener("keyup", this.onKeyPress);
  }

  render() {
    const { app, dismiss, addSetter } = this.props;
    const setters = app.data.adminAccountCollection.ids.get([]).map((accountId, index) => {
      const accountC = Avers.lookupContent<Account>(app.data.aversH, accountId);
      return accountC
        .fmap(() => (
          <div key={index} style={{ marginRight: 8 }}>
            <BoulderSetterCard
              app={app}
              setterId={accountId}
              onClick={() => {
                addSetter(accountId);
              }}
            />
          </div>
        ))
        .get(<div />);
    });

    return (
      <div
        style={{
          position: "fixed",
          zIndex: 10,
          top: 0,
          right: 0,
          bottom: 0,
          left: 0,
          padding: 60,
          background: "#f1f1f1"
        }}
      >
        <div style={{ position: "fixed", top: 20, right: 20 }} onClick={dismiss}>
          <svg width="40" height="40">
            <path d="M10 10 L30 30 M30 10 L10 30" stroke="black" strokeWidth="7" strokeLinecap="round" />
          </svg>
        </div>

        <div style={{ textAlign: "center", marginBottom: 60, fontSize: 32 }}>Pick a setter</div>

        <div style={{ display: "flex", flexWrap: "wrap" }}>{setters}</div>
      </div>
    );
  }
}

// ----------------------------------------------------------------------------

const Section = styled.div`
${useTypeface(copy16Bold)}
color: ${text};

padding: 40px 0 12px;
&:first-of-type {
    padding: 0 0 12px;
}
`;

const SectionLabel = styled.div`
${useTypeface(copy14)}
color: ${darkGrey};

padding: 0 0 4px;
`;

const DangerButton = styled(Button)``;

// ----------------------------------------------------------------------------

const DayPickerWrapper = styled.div`
  & .DayPicker {
    background: white;
    border: 1px solid ${primary};
  }

  & .DayPicker-Day {
    width: 36px;
  }

  & .DayPicker-Day--today {
    font-weight: initial;
  }
`;
