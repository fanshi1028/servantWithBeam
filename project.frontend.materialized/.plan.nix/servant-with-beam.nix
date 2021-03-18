{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { ghcid = false; frontend = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "servant-with-beam"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "Francis Chan (c) 2020-2021";
      maintainer = "jackychany321@gmail.com";
      author = "Francis Chan";
      homepage = "https://github.com/fanshi1028/servantWithBeam";
      url = "";
      synopsis = "Web app";
      description = "Web app";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."universum" or (errorHandler.buildDepError "universum"))
          (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."chronos" or (errorHandler.buildDepError "chronos"))
          (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
          (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
          (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
          (hsPkgs."servant-errors" or (errorHandler.buildDepError "servant-errors"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-pool" or (errorHandler.buildDepError "unliftio-pool"))
          (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
          (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
          (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."servant-auth" or (errorHandler.buildDepError "servant-auth"))
          (hsPkgs."servant-auth-client" or (errorHandler.buildDepError "servant-auth-client"))
          (hsPkgs."servant-auth-docs" or (errorHandler.buildDepError "servant-auth-docs"))
          (hsPkgs."servant-docs" or (errorHandler.buildDepError "servant-docs"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."password" or (errorHandler.buildDepError "password"))
          (hsPkgs."zxcvbn-hs" or (errorHandler.buildDepError "zxcvbn-hs"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          (hsPkgs."validity" or (errorHandler.buildDepError "validity"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."addy" or (errorHandler.buildDepError "addy"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."natural-transformation" or (errorHandler.buildDepError "natural-transformation"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = if flags.frontend then false else true;
        modules = [
          "Controllers"
          "Controllers/ErasedMarks"
          "Controllers/Handlers"
          "Controllers/Hitmen"
          "Controllers/Marks"
          "Controllers/PursuingMarks"
          "Databases/HitmenBusiness"
          "Databases/HitmenBusiness/ErasedMarks"
          "Databases/HitmenBusiness/Handlers"
          "Databases/HitmenBusiness/Hitmen"
          "Databases/HitmenBusiness/Marks"
          "Databases/HitmenBusiness/PursuingMarks"
          "Databases/HitmenBusiness/Utils/Auth"
          "Databases/HitmenBusiness/Utils/Chronos"
          "Databases/HitmenBusiness/Utils/Email"
          "Databases/HitmenBusiness/Utils/JSON"
          "Databases/HitmenBusiness/Utils/Password"
          "Databases/HitmenBusiness/Utils/Types"
          "Models"
          "Models/HitmenBusiness"
          "Servers"
          "Servers/Home"
          "Utils/Account"
          "Utils/Account/Auth"
          "Utils/Account/Login"
          "Utils/Account/SignUp"
          "Utils/Client"
          "Utils/Constraints"
          "Utils/CRUD"
          "Utils/CRUD/CreateRoute"
          "Utils/CRUD/DeleteRoute"
          "Utils/CRUD/ReadRoute"
          "Utils/CRUD/UpdateRoute"
          "Utils/Docs"
          "Utils/FromAccount"
          "Utils/Meta"
          "Utils/QueryRunner"
          "Utils/Types"
          ];
        hsSourceDirs = [ "backend/src" ];
        };
      exes = {
        "app" = {
          depends = [
            (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ] ++ (if flags.ghcid
            then [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."universum" or (errorHandler.buildDepError "universum"))
              (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."chronos" or (errorHandler.buildDepError "chronos"))
              (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-errors" or (errorHandler.buildDepError "servant-errors"))
              (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
              (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
              (hsPkgs."unliftio-pool" or (errorHandler.buildDepError "unliftio-pool"))
              (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
              (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
              (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
              (hsPkgs."servant-auth" or (errorHandler.buildDepError "servant-auth"))
              (hsPkgs."servant-auth-client" or (errorHandler.buildDepError "servant-auth-client"))
              (hsPkgs."servant-auth-docs" or (errorHandler.buildDepError "servant-auth-docs"))
              (hsPkgs."servant-docs" or (errorHandler.buildDepError "servant-docs"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."password" or (errorHandler.buildDepError "password"))
              (hsPkgs."zxcvbn-hs" or (errorHandler.buildDepError "zxcvbn-hs"))
              (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
              (hsPkgs."validity" or (errorHandler.buildDepError "validity"))
              (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."addy" or (errorHandler.buildDepError "addy"))
              (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
              (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
              (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
              (hsPkgs."natural-transformation" or (errorHandler.buildDepError "natural-transformation"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
              ]
            else [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."universum" or (errorHandler.buildDepError "universum"))
              (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."chronos" or (errorHandler.buildDepError "chronos"))
              (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-errors" or (errorHandler.buildDepError "servant-errors"))
              (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
              (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
              (hsPkgs."unliftio-pool" or (errorHandler.buildDepError "unliftio-pool"))
              (hsPkgs."servant-with-beam" or (errorHandler.buildDepError "servant-with-beam"))
              ]);
          buildable = if flags.frontend then false else true;
          hsSourceDirs = [
            "backend/app"
            ] ++ (pkgs.lib).optional (flags.ghcid) "backend/src";
          mainPath = ([ "Main.hs" ] ++ [
            ""
            ]) ++ (pkgs.lib).optional (flags.frontend) "";
          };
        "frontend" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."universum" or (errorHandler.buildDepError "universum"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."chronos" or (errorHandler.buildDepError "chronos"))
            (hsPkgs."clay" or (errorHandler.buildDepError "clay"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."reflex" or (errorHandler.buildDepError "reflex"))
            (hsPkgs."reflex-dom" or (errorHandler.buildDepError "reflex-dom"))
            ];
          buildable = if compiler.isGhc && (compiler.version).ge "8.10.1" || !flags.frontend
            then false
            else true;
          hsSourceDirs = [ "frontend/app" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.10.1" || !flags.frontend) "";
          };
        "scripts" = {
          depends = (pkgs.lib).optionals (!system.isWindows) (if flags.ghcid
            then [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."universum" or (errorHandler.buildDepError "universum"))
              (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."chronos" or (errorHandler.buildDepError "chronos"))
              (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-errors" or (errorHandler.buildDepError "servant-errors"))
              (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
              (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
              (hsPkgs."unliftio-pool" or (errorHandler.buildDepError "unliftio-pool"))
              (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
              (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
              (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
              (hsPkgs."servant-auth" or (errorHandler.buildDepError "servant-auth"))
              (hsPkgs."servant-auth-client" or (errorHandler.buildDepError "servant-auth-client"))
              (hsPkgs."servant-auth-docs" or (errorHandler.buildDepError "servant-auth-docs"))
              (hsPkgs."servant-docs" or (errorHandler.buildDepError "servant-docs"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."password" or (errorHandler.buildDepError "password"))
              (hsPkgs."zxcvbn-hs" or (errorHandler.buildDepError "zxcvbn-hs"))
              (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
              (hsPkgs."validity" or (errorHandler.buildDepError "validity"))
              (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."addy" or (errorHandler.buildDepError "addy"))
              (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
              (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
              (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
              (hsPkgs."natural-transformation" or (errorHandler.buildDepError "natural-transformation"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
              (hsPkgs."beam-automigrate" or (errorHandler.buildDepError "beam-automigrate"))
              (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
              (hsPkgs."optparse-generic" or (errorHandler.buildDepError "optparse-generic"))
              ]
            else [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."universum" or (errorHandler.buildDepError "universum"))
              (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
              (hsPkgs."beam-automigrate" or (errorHandler.buildDepError "beam-automigrate"))
              (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
              (hsPkgs."chronos" or (errorHandler.buildDepError "chronos"))
              (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
              (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
              (hsPkgs."optparse-generic" or (errorHandler.buildDepError "optparse-generic"))
              (hsPkgs."password" or (errorHandler.buildDepError "password"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
              (hsPkgs."unliftio-pool" or (errorHandler.buildDepError "unliftio-pool"))
              (hsPkgs."servant-with-beam" or (errorHandler.buildDepError "servant-with-beam"))
              ]);
          buildable = (if !system.isWindows
            then false
            else true) && (if flags.frontend then false else true);
          modules = (pkgs.lib).optionals (!system.isWindows) [
            "Migration/Databases/HitmenBusiness"
            "Migration/Databases/HitmenBusiness/Handlers"
            "Migration/Utils/Chronos"
            "Migration/Utils/Types"
            ];
          hsSourceDirs = [
            "scripts"
            ] ++ (pkgs.lib).optionals (!system.isWindows) ((pkgs.lib).optional (flags.ghcid) "backend/src");
          mainPath = ([
            "Main.hs"
            ] ++ (pkgs.lib).optionals (!system.isWindows) ([ "" ] ++ [
            ""
            ])) ++ (pkgs.lib).optional (flags.frontend) "";
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."fakedata" or (errorHandler.buildDepError "fakedata"))
            (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
            (hsPkgs."genvalidity-aeson" or (errorHandler.buildDepError "genvalidity-aeson"))
            (hsPkgs."genvalidity-hspec" or (errorHandler.buildDepError "genvalidity-hspec"))
            (hsPkgs."genvalidity-hspec-aeson" or (errorHandler.buildDepError "genvalidity-hspec-aeson"))
            (hsPkgs."genvalidity-property" or (errorHandler.buildDepError "genvalidity-property"))
            (hsPkgs."genvalidity-text" or (errorHandler.buildDepError "genvalidity-text"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-corpus" or (errorHandler.buildDepError "hedgehog-corpus"))
            (hsPkgs."hedgehog-fakedata" or (errorHandler.buildDepError "hedgehog-fakedata"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."servant-quickcheck" or (errorHandler.buildDepError "servant-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-rerun" or (errorHandler.buildDepError "tasty-rerun"))
            ] ++ (if flags.ghcid
            then [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."universum" or (errorHandler.buildDepError "universum"))
              (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."chronos" or (errorHandler.buildDepError "chronos"))
              (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-errors" or (errorHandler.buildDepError "servant-errors"))
              (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
              (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
              (hsPkgs."unliftio-pool" or (errorHandler.buildDepError "unliftio-pool"))
              (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
              (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
              (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
              (hsPkgs."servant-auth" or (errorHandler.buildDepError "servant-auth"))
              (hsPkgs."servant-auth-client" or (errorHandler.buildDepError "servant-auth-client"))
              (hsPkgs."servant-auth-docs" or (errorHandler.buildDepError "servant-auth-docs"))
              (hsPkgs."servant-docs" or (errorHandler.buildDepError "servant-docs"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."password" or (errorHandler.buildDepError "password"))
              (hsPkgs."zxcvbn-hs" or (errorHandler.buildDepError "zxcvbn-hs"))
              (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
              (hsPkgs."validity" or (errorHandler.buildDepError "validity"))
              (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."addy" or (errorHandler.buildDepError "addy"))
              (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
              (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
              (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
              (hsPkgs."natural-transformation" or (errorHandler.buildDepError "natural-transformation"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
              ]
            else [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."universum" or (errorHandler.buildDepError "universum"))
              (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."chronos" or (errorHandler.buildDepError "chronos"))
              (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."servant-auth-server" or (errorHandler.buildDepError "servant-auth-server"))
              (hsPkgs."servant-errors" or (errorHandler.buildDepError "servant-errors"))
              (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
              (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
              (hsPkgs."unliftio-pool" or (errorHandler.buildDepError "unliftio-pool"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-with-beam" or (errorHandler.buildDepError "servant-with-beam"))
              ]);
          buildable = if flags.frontend then false else true;
          modules = [ "Client" ];
          hsSourceDirs = [ "tests" ] ++ (pkgs.lib).optionals (flags.ghcid) [
            "backend/src"
            "backend/app"
            ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }