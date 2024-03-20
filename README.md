# Ada AST to GraphDB

[![Conventional Commits](https://img.shields.io/badge/Conventional%20Commits-1.0.0-informational.svg?style=for-the-badge)](https://conventionalcommits.org)

The goal of this project is to transform an fully decorated AST to a Code Property Graph.

The current implementation using Asis-4-Gnat to create and read the AST from GCC (GNAT), and Neo4j for the Database.

## Installation

Before starting, you need a working environment of Ada compiler. You shall use [GNAT Community 2019](https://www.adacore.com/download) because it is the latest supported version of GNAT that include ASIS, that is currently used by the project. You can also use the latest version of GNAT Pro, with the latest ASIS version available.

Add the following environment variable into your configuration (`.bashrc`, `.zshrc`, etc.):

```bash
GPR_PROJECT_PATH="$GPR_PROJECT_PATH:/<path-to-root-of-the-project>/lib/Asiscomps:/<path-to-root-of-the-project>/lib/Comps"
```

Clone this project.

Then go to the root of the project.

Run the following command:

```bash
gprbuild -p -P atgdb.gpr
```

### Gnat Studio

For the resolution of `Comps` and `Asiscomps` in Gnat Studio, you have to add the following environment variable before runing Gnat Studio:

```bash
GPR_PROJECT_PATH="$GPR_PROJECT_PATH:/<path-to-root-of-the-project>/lib/Asiscomps:/<path-to-root-of-the-project>/lib/Comps" gps
```

This can be added into the shortcut of GNAT Studio (`*.desktop` for linux, etc.)

### Neo4j

The simplest way to use Neo4j with this project is to use [Neo4j Desktop](https://neo4j.com/download/).

After the installation complete, run the app.

Click on "Add Database" > "Create a Local Database".

Set a name, a password, select a version (Tested with Neo4j engine version `>=4.X.Y` and `>=5.X.Y`).

Copy the `sample.env` into `bin` and rename it `.env`. Edit this file to set the password that you have used to create the Neo4j Database.

## Usage

You can run `atgdb` (located in `bin` folder) on source provided in examples.

Example: 

```bash
cd examples/adactl/wks
../../../bin/atgdb -dvx t_max_primitives.ads
```

You can take a look at the `examples/adactl/requests.cyp` file queries, to found queries that can be used in Neo4j Desktop.
