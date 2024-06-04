import React, { useEffect } from "react";
import Layout from "@theme/Layout";

const Home = () => {
  useEffect(() => {
    window.location.href += "docs";
  });
  return <Layout></Layout>;
};

export default Home;
