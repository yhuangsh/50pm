import React from 'react';
import styled from 'styled-components';

import TopBar from '../components/TopBar';

// Constants

// Stylings

const Frame = styled.div`
  width: 100%;
`;

// Components

const AppView = () => {
  return (
    <Frame>
      <TopBar />
    </Frame>
  );
}

export default AppView;

// Utilities