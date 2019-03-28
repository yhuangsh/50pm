
import React from 'react';
import styled from 'styled-components';
import Equation from './Equation';

// Constants

// Component Stylings

const Frame = styled.div`
  height: 100%;
`;

const EmailLine = styled.p`
  font-family: 'Roboto', sans-serif;
  font-size: 50%;
  font-weight: 100;
  text-align: center;
`;

const Grid = styled.div`
  /* self */
  margin-top: 5mm;

  /* children */
  display: grid;
  grid-template-rows: repeat(25, 1fr);
  grid-row-gap: 3ex;
  grid-template-columns: 1fr 1fr ;
  align-items: center;
  justify-items: center;
  justify-content: space-around;

  @media print {
    margin-top: 20mm;
  }
`;

// Component Compositions

const EquationList = (props) => {
  let equs = props.equs;

  console.log("EquationList props.equs = ", equs);

  return (
    <Frame>
      <EmailLine>www.davidhuang.top/50pm</EmailLine>
      <Grid>
        {equs.map(x => <Equation key={x.id} equ={x.equ}/>)}
      </Grid>
    </Frame>
  );
}

export default EquationList;

// Untility Functions
