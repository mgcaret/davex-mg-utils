#!/bin/bash
END_FENCE=45056

if [ -z "${1}" ]; then
  cat <<USAGE_END
Usage: $0 <command> <args>

<command>+<args> should be an ld65 command line with exactly one object
file containing

Appends --start-addr <address> to <command> if the first object file in
<command> has X_DX_AUTO_LOAD exported.  Such a command should probably
only have one object file, one CODE segment, and no other initialized
segments.

<address> is calculated to be a page boundary that causes the linked
object file in question to end somewhere between $AF00 and $AFFF.

od65 should be in the same location as <command>
USAGE_END
  exit 1
fi
shopt -s extglob

COMMW=`which ${1}`
if [ "${?}" -gt 0 ]; then
  echo "Cannot locate ${1}"
  exit 2
fi
OD65="`dirname ${COMMW}`/od65"
if [ ! -x "${OD65}" ]; then
  echo "Cannot use ${OD65}"
  exit 3
fi

CMD_SUFFIX=
OBJECT_FILE=
for ARG in ${*}; do
  # echo "${ARG}"
  if [[ "${ARG}" == @(\-*) ]]; then
    :
    # echo "Skipping ${ARG}"
  else
    if [ -r "${ARG}" ]; then
      ${OD65} --dump-exports ${ARG} | grep -q X_DX_AUTO_LOAD
      RESULT="${?}"
      if [ "${RESULT}" -eq 0 ]; then
        OBJECT_FILE="${ARG}"
      else
        :
        # echo "No X_DX_AUTO_LOAD found in ${ARG}: ${RESULT}"
      fi
    fi
  fi
done
if [ -z "${OBJECT_FILE}" ]; then
  echo "X_DX_AUTO_LOAD not detected, not auto-originating"
else
  echo "Auto-originating ${OBJECT_FILE}"
  CODE_SIZE=`${OD65} -S ${OBJECT_FILE} | awk '/CODE:/{print $2}'`
  if [ -z "${CODE_SIZE}" -o "${CODE_SIZE}" -lt 1 ]; then
    echo "Could not determine code size, got '${CODE_SIZE}'!"
    exit 4
  fi
  echo "Code size: ${CODE_SIZE}"
  RESERVE=0
  RESV=`${OD65} --dump-exports ${OBJECT_FILE} | awk -F'(' 'f{print substr(\$2,1,length(\$2)-1);f=0} /X_DX_RESV/{f=1}'`
  if [ -n "${RESV}" ]; then
    RESERVE="${RESV}"
    echo "Reserve: ${RESERVE}"
  fi
  START=$(( (${END_FENCE} - ${CODE_SIZE} - ${RESERVE}) / 256 * 256 ))
  if [ -z "${START}" ]; then
    echo "Could not calculate a start address!"
    exit 4
  else
    CMD_SUFFIX="${CMD_SUFFIX} --start-addr ${START}"
  fi
fi
CMD="${*}${CMD_SUFFIX}"
echo "${CMD}"
${CMD}

