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

# endpoint '/v1/followup_questions' works

    [
      {
        "question_id": [
          "Q7078_1"
        ],
        "question_text": [
          "Ist für Ihre Tätigkeit in der Regel eine abgeschlossene Berufsausbildung als Friseur/in erforderlich?"
        ],
        "type": [
          "anforderungsniveau"
        ],
        "answers": [
          {
            "answer_id": 1,
            "answer_text": "Nein",
            "last_question": true
          },
          {
            "answer_id": 2,
            "answer_text": "Ja",
            "last_question": true
          }
        ]
      }
    ]

# endpoint '/v1/final_codes' works (without followup answers)

    {
      "isco_08": [
        "5141"
      ],
      "kldb_10": [
        "82312"
      ]
    }

# endpoint '/v1/final_codes' works (with followup answers)

    {
      "isco_08": [
        "9629"
      ],
      "kldb_10": [
        "82311"
      ]
    }

