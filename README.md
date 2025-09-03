# 🚇 Paris Metro Network Visualization

## 📖 Overview
This project demonstrates how to use **OSMnx**, **NetworkX**, and **Matplotlib** to visualize a simplified version of the **Paris Métro system**.  
It fetches real-world coordinates of metro stations, builds a graph of metro lines, calculates distances between connected stations, and plots the network with color-coded lines.

---

## ✨ Features
- Fetch station coordinates using **OSMnx** (OpenStreetMap data).  
- Build a metro graph with **NetworkX**.  
- Compute **great-circle distances** between stations using **Geopy**.  
- Plot Paris Métro lines with **Matplotlib**, including station names and distances.  
- Support for multiple lines with unique colors.  

---

## 🛠️ Installation
Clone this repo and install dependencies:

```bash
git clone https://github.com/your-username/paris-metro-visualization.git
cd paris-metro-visualization
pip install -r requirements.txt
