import React, { useState } from 'react';
import styled from 'styled-components';

import * as g from '../lib/g';

// Constants

// Stylings

const Frame = styled.div`
  margin: 1ex .5em;
  border: 1px solid black;

  display: flex;
  flex-flow: column;
  align-items: center;

  @media print {
    display: none;
  }
`;

const Title = styled.h3`
  flex: 1;

  font-family: 'Fira Code', monospace;
  font-weight: 100;

  text-align: center;
`;

const OptionGroup = styled.div`
  width: 80%;
  flex: 1;
  
  display: flex;
  flex-flow: row nowrap;
  justify-items: space-around;
`;

const Option = styled.h4`
  flex: 1;

  color: ${props => props.enable ? 'black' : 'grey'};
  font-family: 'Fira Code', monospace;
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
    <React.Fragment>
      <Title>...Digits...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickDigit(e, D1)}>1 digit</Option>
        <Option enable={opt[1]} onClick={e => props.onClickDigit(e, D2)}>2 digits</Option>
        <Option enable={opt[2]} onClick={e => props.onClickDigit(e, D3)}>3 digits</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const UnknownOptions = (props) => {
  const U1 = [true, false, false];
  const U2 = [false, true, false];
  const U3 = [false, false, true];

  let opt = props.unknownOpt;

  return (
    <React.Fragment>
      <Title>...Unknowns...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickUnknown(e, U1)}>Left</Option>
        <Option enable={opt[1]} onClick={e => props.onClickUnknown(e, U2)}>Right</Option>
        <Option enable={opt[2]} onClick={e => props.onClickUnknown(e, U3)}>Both</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const SettingsPanel = (props) => {
  return (
    <Frame>
      <DigitOptions {...props} />
      <UnknownOptions {...props}/>
    </Frame>
  );
}

export default SettingsPanel;

// Utilities