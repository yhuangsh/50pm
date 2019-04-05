import React from 'react';
import styled from 'styled-components';

// Constants

// Stylings

const Frame = styled.div`
  margin: 1ex .5em;
 /* border: 1px solid black;*/

  display: flex;
  flex-flow: column nowrap;
  align-items: center;

  @media print {
    display: none;
  }
`;

const OptionGroup = styled.div`
  width: 80%;
  flex: 1;
  
  display: flex;
  flex-flow: row nowrap;
  justify-items: space-around;
  align-items: baseline;
`;

const Title = styled.h3`
  flex: 1;

  font-family: 'Montserrat', sans-serif;
  font-weight: 100;

  text-align: left;
`;

const Option = styled.h4`
  flex: 1;

  color: ${props => props.enable ? 'black' : 'grey'};
  font-size: ${props => props.enable ? '110%' : '100%'};

  font-family: 'Dosis', cursive;
  font-weight: 100;

  text-align: center;

  :hover {
    cursor: pointer;
  }
`;

// Components

const DigitOptions = (props) => {
  const D1 = [true, false, false];
  const D2 = [false, true, false];
  const D3 = [false, false, true];

  let opt = props.digitOpt;

  return (
    <OptionGroup>
      <Title>Digits</Title>
      <Option enable={opt[0]} onClick={e => props.onClickDigit(e, D1)}>1 digit</Option>
      <Option enable={opt[1]} onClick={e => props.onClickDigit(e, D2)}>2 digits</Option>
      <Option enable={opt[2]} onClick={e => props.onClickDigit(e, D3)}>3 digits</Option>
    </OptionGroup>
  );
}

const UnknownOptions = (props) => {
  const U1 = [true, false, false];
  const U2 = [false, true, false];
  const U3 = [false, false, true];

  let opt = props.unknownOpt;

  return (
    <OptionGroup>
      <Title>Unknowns on</Title>
      <Option enable={opt[0]} onClick={e => props.onClickUnknown(e, U1)}>Left side</Option>
      <Option enable={opt[1]} onClick={e => props.onClickUnknown(e, U2)}>Right side</Option>
      <Option enable={opt[2]} onClick={e => props.onClickUnknown(e, U3)}>Both sides</Option>
    </OptionGroup>
  );
}

const PageOptions = (props) => {
  const P1 = [true, false, false, false];
  const P2 = [false, true, false, false];
  const P3 = [false, false, true, false];
  const P4 = [false, false, false, true];

  let opt = props.pageOpt;

  return (
    <OptionGroup>
      <Title>Pages</Title>
      <Option enable={opt[0]} onClick={e => props.onClickPage(e, P1)}>1 page</Option>
      <Option enable={opt[1]} onClick={e => props.onClickPage(e, P2)}>2 pages</Option>
      <Option enable={opt[2]} onClick={e => props.onClickPage(e, P3)}>4 pages</Option>
      <Option enable={opt[3]} onClick={e => props.onClickPage(e, P4)}>10 pages</Option>
    </OptionGroup>
  );
}

const SettingsPanel = (props) => {
  return (
    <Frame>
      <DigitOptions {...props} />
      <UnknownOptions {...props} />
      <PageOptions {...props} />
    </Frame>
  );
}

export default SettingsPanel;

// Utilities