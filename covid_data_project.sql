
-- Updating table to change blank spaces into NULL values

UPDATE covid_data..covid_deaths 
SET continent = NULLIF(continent, '')

UPDATE covid_data..covid_vaccinations 
SET new_vaccinations = NULLIF(new_vaccinations, '')

-- SELECT *
-- FROM covid_data..covid_vaccinations
-- ORDER BY 3,4

--Select data that I am going to be using

SELECT Location, date, total_cases, new_cases, total_deaths, population
FROM covid_data..covid_deaths
WHERE continent IS NOT NULL 
ORDER BY 1,2

-- Looking at total cases vs total deaths
-- Shows likelihood of dying if you contract COVID in your country

SELECT Location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as death_percentage
FROM covid_data..covid_deaths
WHERE location LIKE '%states%'
AND continent IS NOT NULL
ORDER BY 1,2

-- Looking at total cases vs population
-- Shows what percentage of population got COVID

SELECT Location, date, population, total_cases, (total_cases/population)*100 as infection_percentage
FROM covid_data..covid_deaths
WHERE continent is NOT NULL
-- WHERE location like '%states%'
ORDER BY 1,2 

-- Looking at countries with highest infection rates compared to population

SELECT Location, population, MAX(total_cases) as highest_infection_count, MAX((total_cases/population))*100 as percent_population_infected
FROM covid_data..covid_deaths
WHERE continent IS NOT NULL
GROUP BY Location, Population
ORDER BY percent_population_infected DESC

-- Showing countries with highest death count per population

SELECT Location, MAX(cast(total_deaths as int)) as total_death_count
FROM covid_data..covid_deaths
WHERE continent IS NOT NULL 
GROUP BY Location
ORDER BY total_death_count DESC

-- Breaking things down by continent
-- Showing continents with highest death count

SELECT continent, MAX(cast(total_deaths as int)) as total_death_count
FROM covid_data..covid_deaths
WHERE continent IS NOT NULL 
AND iso_code NOT LIKE '%C'
GROUP BY continent
ORDER BY total_death_count DESC

-- Global numbers

SELECT SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as death_percentage--, total_deaths, (total_deaths/total_cases)*100 as death_percentage
FROM covid_data..covid_deaths
WHERE continent IS NOT NULL
--GROUP BY date
ORDER BY 1,2

-- Examining total population vs vaccinations

SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(Cast(vac.new_vaccinations as numeric)) OVER (Partition by dea.location ORDER BY dea.location,
dea.date) as rolling_people_vaccinated
--, (rolling_people_vaccinated/population)*100
FROM covid_data..covid_deaths dea
JOIN covid_data..covid_vaccinations vac
	ON dea.location = vac.location
	AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
ORDER BY 2,3

-- Use CTE
WITH popvsvac (continent, location, date, population, new_vaccinations, rolling_people_vaccinated)
AS
(
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(Cast(vac.new_vaccinations as numeric)) OVER (Partition by dea.location ORDER BY dea.location,
dea.date) as rolling_people_vaccinated
--, (rolling_people_vaccinated/population)*100
FROM covid_data..covid_deaths dea
JOIN covid_data..covid_vaccinations vac
	ON dea.location = vac.location
	AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2,3
)
SELECT *, (rolling_people_vaccinated/population)*100
FROM popvsvac

-- TEMP TABLE

DROP TABLE IF EXISTS #percent_population_vaccinated
CREATE TABLE #percent_population_vaccinated
(
continent nvarchar(255),
location nvarchar(255),
date datetime,
population numeric,
new_vaccinations numeric,
rolling_people_vaccinated numeric
)

INSERT INTO #percent_population_vaccinated
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(Cast(vac.new_vaccinations as numeric)) OVER (Partition by dea.location ORDER BY dea.location,
dea.date) as rolling_people_vaccinated
--, (rolling_people_vaccinated/population)*100
FROM covid_data..covid_deaths dea
JOIN covid_data..covid_vaccinations vac
	ON dea.location = vac.location
	AND dea.date = vac.date
WHERE dea.continent IS NOT NULL

SELECT *, (rolling_people_vaccinated/population)*100
FROM #percent_population_vaccinated

-- Creating view to store data for later visualizations

CREATE VIEW percent_population_vaccinated AS
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(Cast(vac.new_vaccinations as numeric)) OVER (Partition by dea.location ORDER BY dea.location,
dea.date) as rolling_people_vaccinated
--, (rolling_people_vaccinated/population)*100
FROM covid_data..covid_deaths dea
JOIN covid_data..covid_vaccinations vac
	ON dea.location = vac.location
	AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2,3