import React from 'react';
import styled from 'styled-components';

import * as defaults from '../lib/defaults';

// Constants

// Stylings

const Frame = styled.div`
  margin: 1ex .5em;

  @media print {
    display: none;
  }
`;

const Title = styled.h4`
  font-family: 'Montserrat', sans-serif;
  font-weight: 100;
  text-align: center;
`;

const OptionGroup = styled.div`  
  display: flex;
  flex-flow: row nowrap;
  justify-items: space-around;
  align-items: baseline;
`;

const Option = styled.h5`
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

  const opt = props.digitOpt;

  return (
    <React.Fragment>
      <Title>... Digits ...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickDigit(e, D1)}>1 digit</Option>
        <Option enable={opt[1]} onClick={e => props.onClickDigit(e, D2)}>2 digits</Option>
        <Option enable={opt[2]} onClick={e => props.onClickDigit(e, D3)}>3 digits</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const DigitOptionsForTimes = (props) => {
  const D1 = [true, false, false];
  const D2 = [false, true, false];
  const D3 = [false, false, true];

  const opt = props.digitOptForTimes;

  return (
    <React.Fragment>
      <Title>... Digits ...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickDigitForTimes(e, D1)}>1 digit</Option>
        <Option enable={opt[1]} onClick={e => props.onClickDigitForTimes(e, D2)}>2 digits x 1 digit</Option>
        <Option enable={opt[2]} onClick={e => props.onClickDigitForTimes(e, D3)}>2 digits x 2 digits</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const UnknownOptions = (props) => {
  const U1 = [true, false, false];
  const U2 = [false, true, false];
  const U3 = [false, false, true];

  const opt = props.unknownOpt;

  return (
    <React.Fragment>
      <Title>... Unknowns on ...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickUnknown(e, U1)}>Left side</Option>
        <Option enable={opt[1]} onClick={e => props.onClickUnknown(e, U2)}>Right side</Option>
        <Option enable={opt[2]} onClick={e => props.onClickUnknown(e, U3)}>Both sides</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const PageOptions = (props) => {
  const P1 = [true, false, false, false];
  const P2 = [false, true, false, false];
  const P3 = [false, false, true, false];
  const P4 = [false, false, false, true];

  let opt = props.pageOpt;

  return (
    <React.Fragment>
      <Title>... Pages ...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickPage(e, P1)}>1 page</Option>
        <Option enable={opt[1]} onClick={e => props.onClickPage(e, P2)}>2 pages</Option>
        <Option enable={opt[2]} onClick={e => props.onClickPage(e, P3)}>4 pages</Option>
        <Option enable={opt[3]} onClick={e => props.onClickPage(e, P4)}>10 pages</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const SettingsPanel = (props) => {
  const { mode, ...restprops } = props;

  return (
    <Frame>
      {(mode === defaults.PM_MODE ? 
        <DigitOptions {...restprops} /> :
        <DigitOptionsForTimes {...restprops} />)}
      <UnknownOptions {...restprops} />
      <PageOptions {...restprops} />
    </Frame>
  );
}

export default SettingsPanel;

// Utilities