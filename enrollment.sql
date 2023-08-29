-- Specify columns of data 
CREATE TABLE enrollment (
	inst_id INT,
    srvy_yr INT,
    term VARCHAR(6),
    yr_term VARCHAR(11),
    univ VARCHAR(100),
    campus VARCHAR(100),
    necs_id INT,
    city VARCHAR(30),
    state_id INT,
    state VARCHAR(2),
    mla_iclevel INT,
    lang_code INT,
    lang VARCHAR(60),
    undergrad_1_2 INT,
    undergrad_3_4 INT,
    undergrad_total INT,
    grad_total INT,
    all_level_total INT,
    lang_region VARCHAR(6),
    other_lang VARCHAR(100),
    geo_code INT,
    n_resp VARCHAR(2),
    zero_erl VARCHAR(1)
);

-- Import data from a tab-separated file (university name history removed)
LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.1/Uploads/enrollment.txt' 
INTO TABLE enrollment
FIELDS TERMINATED BY '\t'
IGNORE 1 LINES
(@vinst_id, srvy_yr, term, yr_term, univ, campus, @vnecs_id, city, @vstate_id,
@vstate, @vmla_iclevel, @vlang_code, @vlang, @vundergrad_1_2, @vundergrad_3_4, @vundergrad_total, 
@vgrad_total, @vall_level_total, lang_region, other_lang, @vgeo_code, n_resp, zero_erl)
SET 
inst_id = NULLIF(@vinst_id,''),
necs_id = NULLIF(@vnecs_id,''),
undergrad_1_2 = NULLIF(@vundergrad_1_2,''),
undergrad_3_4 = NULLIF(@vundergrad_3_4,''),
undergrad_total = NULLIF(@vundergrad_total,''),
grad_total = NULLIF(@vgrad_total,''),
all_level_total = NULLIF(@vall_level_total,''),
state = NULLIF(@vstate,''),
state_id = NULLIF(@vstateid,''),
mla_iclevel = NULLIF(@vmla_iclevel,''),
geo_code = NULLIF(@vgeo_code,''),
lang_code = NULLIF(@vlang_code,''),
lang = NULLIF(@vlang,'')
;

-- Find the which terms were surveyed for the data
SELECT DISTINCT term
FROM enrollment;

-- Find enrollment numbers for each year and term
SELECT srvy_yr, term, SUM(all_level_total) AS total_enrollment
FROM enrollment
GROUP BY srvy_yr, term
ORDER BY srvy_yr, term;
/* Only 3 summer terms were captured.
All other years only have data for the fall term. */


-- What were the total foreign language enrollment numbers in the fall per year?
SELECT srvy_yr, SUM(all_level_total) AS total_enrollment
FROM enrollment
WHERE term = 'Fall'
GROUP BY srvy_yr
ORDER BY srvy_yr;

-- What was the most popular foreign language in each state in 2016?
WITH total AS (
	SELECT state, lang, SUM(all_level_total) AS total_enrollment
	FROM enrollment
	WHERE srvy_yr = 2016 AND term = 'Fall'
	GROUP BY state, lang
)
SELECT total.state, total.lang, highest.total_enrollment
FROM total
RIGHT JOIN (
	SELECT total.state, MAX(total_enrollment) AS total_enrollment
	FROM total
	GROUP BY state
) AS highest ON total.state = highest.state AND total.total_enrollment = highest.total_enrollment
ORDER BY state;

-- What was the most popular foreign language besides Spanish in each state in 2016?
WITH total AS (
	SELECT state, lang, SUM(all_level_total) AS total_enrollment
	FROM enrollment
	WHERE srvy_yr = 2016 AND term = 'Fall' AND lang <> 'SPANISH'
	GROUP BY state, lang
)
SELECT total.state, total.lang, highest.total_enrollment
FROM total
RIGHT JOIN (
	SELECT total.state, MAX(total_enrollment) AS total_enrollment
	FROM total
	GROUP BY state
) AS highest ON total.state = highest.state AND total.total_enrollment = highest.total_enrollment
ORDER BY state;

-- How many schools offered Finnish each year?
SELECT srvy_yr, COUNT(univ)
FROM enrollment
WHERE lang = 'Finnish' AND term = 'Fall'
GROUP BY srvy_yr
ORDER BY srvy_yr;

-- Which languages have the highest enrollment numbers?
SELECT lang, SUM(all_level_total) AS total
FROM enrollment
GROUP BY lang
ORDER BY total DESC;

-- What are the enrollment numbers for the top languages each year?
SELECT srvy_yr, lang, SUM(all_level_total)
FROM enrollment
WHERE lang IN ('SPANISH', 'FRENCH', 'GERMAN', 'ITALIAN', 'RUSSIAN') AND term = 'Fall'
GROUP BY srvy_yr, lang;

-- Which languages were offered in all 50 states and DC in 1974?
SELECT lang
FROM (
	SELECT lang, COUNT(DISTINCT state) AS num_states
	FROM enrollment
	WHERE srvy_yr = 1974 AND all_level_total > 0
	GROUP BY lang
) AS lang_num_states
WHERE lang_num_states.num_states = 51;

-- Which languages were offered in all 50 states and DC in 2009?
SELECT lang
FROM (
	SELECT lang, COUNT(DISTINCT state) AS num_states
	FROM enrollment
	WHERE srvy_yr = 2009 AND all_level_total > 0
	GROUP BY lang
) AS lang_num_states
WHERE lang_num_states.num_states = 51;

-- Which languages were offered in all 50 states and DC in 2016?
SELECT lang
FROM (
	SELECT lang, COUNT(DISTINCT state) AS num_states
	FROM enrollment
	WHERE srvy_yr = 2016 AND all_level_total > 0
	GROUP BY lang
) AS lang_num_states
WHERE lang_num_states.num_states = 51;

-- How many languages were offered in all 50 states and DC each year?
SELECT srvy_yr, COUNT(DISTINCT all_state_lang.lang)
FROM (
	SELECT srvy_yr, lang
	FROM (
		SELECT srvy_yr, lang, COUNT(DISTINCT state) AS num_states
		FROM enrollment
        WHERE all_level_total > 0
		GROUP BY srvy_yr, lang
		ORDER BY num_states DESC
	) AS lang_num_states
	WHERE lang_num_states.num_states = 51
) AS all_state_lang
GROUP BY srvy_yr
ORDER BY srvy_yr;

/* How many different languages were offered in each state at any time from 1974 to 2016? 
Note that we do not have data for all the years in this time range. */
SELECT state, COUNT(DISTINCT lang) AS num_languages
FROM enrollment
WHERE all_level_total > 0 AND srvy_yr >= 1974
GROUP BY state;

-- What are the top 5 schools in Massachusetts in terms of languages offered?
SELECT univ, COUNT(DISTINCT lang) AS num_lang
FROM enrollment
WHERE state = 'MA'
GROUP BY univ
ORDER BY num_lang DESC
LIMIT 5;

-- What are the enrollment numbers of ancient Greek vs modern Greek each year?
SELECT modern.srvy_yr, modern_greek_enrollment, ancient_greek_enrollment
FROM (
	SELECT srvy_yr, SUM(all_level_total) AS modern_greek_enrollment
	FROM enrollment
	WHERE lang = '"GREEK, MODERN"'
	GROUP BY srvy_yr
) AS modern
INNER JOIN
(
	SELECT srvy_yr, SUM(all_level_total) AS ancient_greek_enrollment
	FROM enrollment
	WHERE lang = '"GREEK, ANCIENT"'
	GROUP BY srvy_yr
) AS ancient
ON modern.srvy_yr = ancient.srvy_yr
ORDER BY modern.srvy_yr;

/* What school had the highest language enrollment in 1980 
among the schools that offered at most 5 languages that year? */
WITH few_lang_univ AS (
	SELECT enrollment.univ, SUM(enrollment.all_level_total) AS total_enrollment
	FROM (
		SELECT *
		FROM (
			SELECT univ, COUNT(DISTINCT lang) AS lang_count
			FROM enrollment
			WHERE srvy_yr = 1980
			GROUP BY univ
		) AS q1
		WHERE q1.lang_count <= 5
	) AS q2
	INNER JOIN enrollment on q2.univ = enrollment.univ
	GROUP BY enrollment.univ
) 
SELECT few_lang_univ.univ, q3.total_enrollment
FROM few_lang_univ
RIGHT JOIN (
	SELECT MAX(total_enrollment) AS total_enrollment
    FROM few_lang_univ
) q3 ON few_lang_univ.total_enrollment = q3.total_enrollment;

-- Which languages had the highest growth rate in enrollment from 2013 to 2016 (min. 500 students in 2013)?
SELECT enrollment2013.lang, enrollment2013.total AS 2013_enrollment, enrollment2016.total AS 2016_enrollment,
100*(enrollment2016.total - enrollment2013.total)/enrollment2013.total AS percent_increase 
FROM (
	SELECT lang, SUM(all_level_total) AS total
	FROM enrollment
	WHERE srvy_yr = 2013 AND all_level_total > 0
	GROUP BY lang
) enrollment2013
INNER JOIN
(
	SELECT lang, SUM(all_level_total) AS total
	FROM enrollment
	WHERE srvy_yr = 2016 AND all_level_total > 0
	GROUP BY lang
) enrollment2016 ON enrollment2013.lang = enrollment2016.lang
WHERE enrollment2013.total >= 500
ORDER BY percent_increase DESC
LIMIT 10;

