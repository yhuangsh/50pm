import React from 'react';
import styled from 'styled-components';

// Constants

// Stylings

const Frame = styled.div`
  margin: 1ex .5em;
  border: 1px solid black;
`;

const Title = styled.h2`

  font-family: 'Fira Code', monospace;
  font-weight: 100;

  text-align: center;
`;

// Components

const TopBar = () => {
  return (
    <Frame>
      <Title>Fifty Plus Minus</Title>
    </Frame>
  );
}

export default TopBar;

// Utilities