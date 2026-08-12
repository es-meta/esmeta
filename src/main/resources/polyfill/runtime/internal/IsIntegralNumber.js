// An integral Number is a Number the language does not name, so the
// integrality has to be tested rather than reported by `typeof`.
function IsIntegralNumber(value) {
  return (
    typeof value === "number" && isFinite(value) && Math.floor(value) === value
  );
}

module.exports = IsIntegralNumber;
