# endpoint '/v1/suggestions' works (with suggestions)

    [
      {
        "auxco_id": "7078",
        "input_text": "Friseur",
        "score": 0.8353,
        "title": "Friseur/in",
        "task": "Färben, Schneiden und Frisieren von Haaren",
        "task_description": "z.B. Kundentermine vereinbaren; Haare nach Anweisung waschen, shampoonieren und föhnen; Frisierplätze und Waschbecken reinigen; nach Kundenwünschen Maßtoupet oder Maßperücke aus Echt- oder Kunsthaar anfertigen; Arbeitsmaterialien reinigen und bereitstellen",
        "kldb_title_short": "Friseurgewerbe",
        "has_followup_questions": true
      },
      {
        "auxco_id": "7086",
        "input_text": "Friseur",
        "score": 0.035,
        "title": "Kosmetikmeister/in",
        "task": "Führungsaufgaben mit Personalverantwortung im kosmetischen Bereich",
        "task_description": "Führungsaufgaben mit Personalverantwortung bedeutet, dass untergeordnete Arbeitskräfte beaufsichtigt und angeleitet werden. Weitere Aufgaben sind z.B. Kunden beraten; im Salon praktisch mitarbeiten; Mitarbeiter fachlich unterstützen; Studioaussattung festlegen; Personaleinsatz planen und bei Aus- und Weiterbildung mitwirken; Kundenkartei und Terminkalender führen; Werbemaßnahmen durchführen",
        "kldb_title_short": "Körperpflege (Führungskraft)",
        "has_followup_questions": false
      },
      {
        "auxco_id": "7085",
        "input_text": "Friseur",
        "score": 0.03,
        "title": "Friseurmeister/in",
        "task": "Führungsaufgaben mit Personalverantwortung im Friseurwesen",
        "task_description": "Führungsaufgaben mit Personalverantwortung bedeutet, dass untergeordnete Arbeitskräfte beaufsichtigt und angeleitet werden. Weitere Aufgaben sind z.B. Kunden hinsichtlich Haarpflege, Trendfrisuren, Typveränderung und kosmetischer Produkte beraten; Haare schneiden und färben; Mitarbeiter fachlich unterstützen; Salonaussattung festlegen; Kundenkartei und Terminkalender führen; Personaleinsatz planen und bei Aus- und Weiterbildung mitwirken",
        "kldb_title_short": "Körperpflege (Führungskraft)",
        "has_followup_questions": false
      },
      {
        "auxco_id": "7087",
        "input_text": "Friseur",
        "score": 0.03,
        "title": "Chefmaskenbildner/in",
        "task": "Führungsaufgaben mit Personalverantwortung in der Maskenbildnerei beim Film, der Oper oder im Theater",
        "task_description": "Führungsaufgaben mit Personalverantwortung bedeutet, dass untergeordnete Arbeitskräfte beaufsichtigt und angeleitet werden. Weitere Aufgaben sind z.B. Kunden beraten; bei Aufträgen praktisch mitarbeiten; Mitarbeiter fachlich unterstützen; Maskenbildnerausstattung festlegen; Personaleinsatz planen und bei Aus- und Weiterbildung mitwirken",
        "kldb_title_short": "Körperpflege (Führungskraft)",
        "has_followup_questions": false
      },
      {
        "auxco_id": "6102",
        "input_text": "Friseur",
        "score": 0.009,
        "title": "Konferenz- und Veranstaltungsplaner",
        "task": "Planung und Organisation von Events, Konzerten, Festivals, Konferenzen, Messen, Feiern oder anderen Großveranstaltungen",
        "task_description": "z.B. Kunden zu geplanten Events beraten und Angebote erstellen; für Konferenzen und Messen bei potenziellen Teilnehmern werben; Anmeldungen entgegennehmen; Räumlichkeiten, Catering, Beschilderung und Transport organisieren; Künstler engagieren und ein Programm planen",
        "kldb_title_short": "Veranstaltungsservice und -management",
        "has_followup_questions": true
      }
    ]

# endpoint '/v1/suggestions' works (w/o suggestions)

    [
      {
        "input_text": "A piece of text that does not tell you anything",
        "score": 0
      }
    ]

# endpoint '/v1/suggestions' works (w/o suggestions due to high threshold)

    [
      {
        "input_text": "Friseur",
        "score": 0
      }
    ]

# endpoint '/v1/suggestions' works (for KldB)

    [
      {
        "input_text": "Friseur",
        "kldb_id": "82312",
        "level": 5,
        "description": "Inhalt: Diese Systematikposition umfasst alle Berufe im Friseurgewerbe, deren Tätigkeiten fundierte fachliche Kenntnisse und Fertigkeiten erfordern. Angehörige dieser Berufe schneiden, färben und frisieren Haare.",
        "excludes": "Nicht einzubeziehende Berufe: Kosmetiker/in (82322) Fachkraft – Beauty und Wellness (82222) Maskenbildner/in (82342)",
        "title": "Berufe im Friseurgewerbe - fachlich ausgerichtete Tätigkeiten",
        "score": 0.8353,
        "sim_name": "wordwise"
      },
      {
        "input_text": "Friseur",
        "kldb_id": "82393",
        "level": 5,
        "description": "Inhalt: Angehörige dieser Berufe übernehmen Aufsichtsaufgaben im Bereich der Körperpflege, die Spezialkenntnisse und -fertigkeiten erfordern. Sie arbeiten in der Regel praktisch im Betrieb mit, führen Dienstleistungen in der Körperpflege durch und beaufsichtigen die Arbeit ihrer Mitarbeiter/innen.",
        "excludes": "Nicht einzubeziehende Berufe: Wellnessmanager/in (82223) Maskenbildner/in (Hochschule) (82343)",
        "title": "Aufsichtskräfte - Körperpflege",
        "score": 0.0901,
        "sim_name": "wordwise"
      },
      {
        "input_text": "Friseur",
        "kldb_id": "62182",
        "level": 5,
        "description": "Inhalt: Diese Systematikposition umfasst alle Berufe im Verkauf (ohne Produktspezialisierung), deren Tätigkeiten fundierte fachliche Kenntnisse und Fertigkeiten erfordern und die in der übergeordneten Systematikposition „621 Verkauf (ohne Produktspezialisier-ung)“ nicht anderweitig erfasst sind. Angehörige dieser Berufe präsentieren und verkaufen Waren und Dienstleistungen, gehen dabei ggf. von Tür zu Tür und führen Waren in Handelseinrichtungen, Messen und privaten Haushalten vor.",
        "excludes": "Nicht einzubeziehende Berufe: Verkaufsberater/in (62102) Sales- und Servicefachkraft (61122) Telefonverkäufer/in (92122)",
        "title": "Berufe im Verkauf (ohne Produktspezialisierung) (sonstige spezifische Tätigkeitsangabe) - fachlich ausgerichtete Tätigkeiten",
        "score": 0.009,
        "sim_name": "wordwise"
      },
      {
        "input_text": "Friseur",
        "kldb_id": "63194",
        "level": 5,
        "description": "Inhalt: Angehörige dieser Berufe übernehmen Führungsaufgaben im Bereich Tourismus und Sport, welche einen hohen Komplexitätsgrad aufweisen und ein entsprechend hohes Kenntnis- und Fertigkeitsniveau erfordern. Angehörige dieser Berufe leiten z.B. ein Reisebüro bzw. -unternehmen, ein Fremdenverkehrsamt oder ein Sportzentrum.",
        "excludes": "Nicht einzubeziehende Berufe: Touristikmanager/in (63114) Sportmanager/in (63124) Wellnessmanager/in (82223) Eventmanager/in (63403) Hoteldirektor/in (63294)",
        "title": "Führungskräfte - Tourismus und Sport",
        "score": 0.009,
        "sim_name": "wordwise"
      },
      {
        "input_text": "Friseur",
        "kldb_id": "63402",
        "level": 5,
        "description": "Inhalt: Diese Systematikposition umfasst alle Berufe im Veranstaltungsservice und -management, deren Tätigkeiten fundierte fachliche Kenntnisse und Fertigkeiten erfordern. Angehörige dieser Berufe unterstützen Veranstaltungsmanager/innen bei ihrer Arbeit und führen meist Assistenztätigkeiten im Bereich der Veranstaltungsorganisation aus.",
        "excludes": "Nicht einzubeziehende Berufe: Kaufmann/-frau – Tourismus und Freizeit (63112) Kaufmännische/r Assistent/in, Wirtschaftsassistent/in – Sportverwaltung und -organisation (63122) Bühnenarbeiter/in (94512)",
        "title": "Berufe im Veranstaltungsservice und -management - fachlich ausgerichtete Tätigkeiten",
        "score": 0.009,
        "sim_name": "wordwise"
      }
    ]

---

    [
      {
        "input_text": "A piece of text that does not tell you anything",
        "score": 0
      }
    ]

# endpoint '/v1/next_followup_question' works

    {
      "question_id": "Q7078_1",
      "question_text": "Ist für Ihre Tätigkeit in der Regel eine abgeschlossene Berufsausbildung als Friseur/in erforderlich?",
      "type": "anforderungsniveau",
      "answers": [
        {
          "answer_id": 1,
          "answer_text": "Nein",
          "corresponding_answer_level": "isco_skill_level_1",
          "coding_is_finished": true
        },
        {
          "answer_id": 2,
          "answer_text": "Ja",
          "corresponding_answer_level": "isco_skill_level_2",
          "coding_is_finished": true
        }
      ]
    }

# endpoint '/v1/next_followup_question' works (when coding is finished)

    {
      "coding_is_finished": true
    }

# endpoint '/v1/next_followup_question' works with multiple followup questions (incl. early end)

    {
      "question_id": "Q5078_1",
      "question_text": "Sind Sie Beamte/r im mittleren, gehobenen oder höheren Dienst?",
      "type": "anforderungsniveau",
      "answers": [
        {
          "answer_id": 1,
          "answer_text": "im mittleren Dienst oder vergleichbar",
          "corresponding_answer_level": "isco_skill_level_2",
          "coding_is_finished": true
        },
        {
          "answer_id": 2,
          "answer_text": "im gehobenen Dienst oder vergleichbar",
          "corresponding_answer_level": "isco_skill_level_3",
          "coding_is_finished": true
        },
        {
          "answer_id": 3,
          "answer_text": "im höheren Dienst oder vergleichbar",
          "corresponding_answer_level": "isco_skill_level_4",
          "coding_is_finished": false
        }
      ]
    }

---

    {
      "coding_is_finished": true
    }

---

    {
      "question_id": "Q5078_2",
      "question_text": "Sind Sie befugt strategische Entscheidungen zu treffen, z.B. zur Einführung neuer Verfahren, zu finanziellen Investitionen oder zur Einstellung und Entlassung von Personal?",
      "type": "aufsicht",
      "answers": [
        {
          "answer_id": 1,
          "answer_text": "ja",
          "corresponding_answer_level": "isco_manager",
          "coding_is_finished": true
        },
        {
          "answer_id": 2,
          "answer_text": "Nein",
          "corresponding_answer_level": "isco_not_supervising",
          "coding_is_finished": true
        }
      ]
    }

---

    {
      "coding_is_finished": true
    }

# endpoint '/v1/final_codes' works (without followup answers)

    {
      "isco_08": "5141",
      "kldb_10": "82312",
      "message": "Returning default code: Improve followup_answers (or standardized_answer_levels) to obtain more exact codings."
    }

# endpoint '/v1/final_codes' works (with followup answers)

    {
      "isco_08": "9629",
      "kldb_10": "82311",
      "message": ""
    }

# endpoint '/v1/final_codes' works (with partial followup answers)

    {
      "isco_08": "2120",
      "kldb_10": "91344",
      "message": "Entry missing for Q1836_2 in followup_answers."
    }

# endpoint '/v1/final_codes' works (with standardized followup answers)

    {
      "isco_08": "3115",
      "kldb_10": "25183",
      "message": "Approximate match: isco_skill_level_1 -> isco_skill_level_3 -> Q1706_1=2"
    }

---

    {
      "isco_08": "3123",
      "kldb_10": "34293",
      "message": "Approximate match: isco_manager -> isco_supervisor -> Q1783_1=1"
    }

# API logging is working

    c("ts_preroute", "ts_postserialize", "execution_time", "req_request_method", 
    "req_path_info", "req_query_string", "req_http_user_agent", "req_remote_addr", 
    "res_status")

