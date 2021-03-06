module.exports = {
  purge: {
    content: ['./src/**/*.elm'],
    safelist: [
      "text-highlight-00",
      "text-highlight-01",
      "text-highlight-02",
      "text-highlight-03",
      "text-highlight-04",
      "text-highlight-05",
      "text-highlight-06",
      "text-highlight-07",
      "text-highlight-08",
      "text-highlight-09",
      "text-highlight-10",
      "text-highlight-11",
      "text-highlight-12",
      "text-highlight-13",
      "text-highlight-14",
      "text-highlight-15",
      "text-highlight-F00",
      "text-highlight-F01",
      "text-highlight-F02",
      "text-highlight-F03",
      "text-highlight-F04",
      "text-highlight-F05",
      "text-highlight-F06",
      "text-highlight-F07",
      "text-highlight-F08",
      "text-highlight-F09",
      "text-highlight-F10",
      "text-highlight-F11",
      "text-highlight-F12",
      "text-highlight-F13",
      "text-highlight-F14",
      "text-highlight-F15"
    ]
  },
  darkMode: false, // or 'media' or 'class'
  theme: {
    colors: {
      background: "#1a1c24",
      foreground: "#d5d8da",
      timestamp: "#555",
      link: "#7693ff",
      input: "#6c6f9380",
      highlight: {
        "00": "#d6deeb",
        "01": "#da0f7a",
        "02": "#22da6e",
        "03": "#addb67",
        "04": "#82aaff",
        "05": "#c792ea",
        "06": "#21c7a8",
        "07": "#22da6e",
        "08": "#da0f7a",
        "09": "#ff217c",
        "10": "#22da6e",
        "11": "#7fdbca",
        "12": "#82aaff",
        "13": "#c792ea",
        "14": "#7fdbca",
        "15": "#22da6e",
        "F00": "#d6deeb",
        "F01": "#da0f7a",
        "F02": "#22da6e",
        "F03": "#addb67",
        "F04": "#82aaff",
        "F05": "#c792ea",
        "F06": "#21c7a8",
        "F07": "#22da6e",
        "F08": "#da0f7a",
        "F09": "#ff217c",
        "F10": "#22da6e",
        "F11": "#7fdbca",
        "F12": "#82aaff",
        "F13": "#c792ea",
        "F14": "#7fdbca",
        "F15": "#22da6e"
      }
    },
    fontFamily: {
      'sans': ['Lato', 'sans-serif']
    },
    extend: {
      spacing: {
        "0.2": "0.2rem",
      }
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
