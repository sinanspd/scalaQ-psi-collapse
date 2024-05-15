f = open("groverSTD2RUN.txt", "r")

statedict = {}

for x in f:
  if "Releasing" in x:
    state = x.split("Vector(", 1)[1]
    stateSplit = state.split(",")
    q0 = "0" if stateSplit[0] == "false" else "1"
    q1 = "0" if stateSplit[1].replace(" ", "") == "false" else "1"
    q2 = "0" if stateSplit[2].replace(")", "").replace(" ", "") == "false" else "1"
    totalstate = q0 + q1 + q2
    if totalstate in statedict:
      currentvalue = statedict[totalstate]
      statedict[totalstate] = currentvalue + 1
    else: 
      statedict[totalstate] = 1


print("Correct: " + str(statedict["101"]))
print("Total: " + str(sum(statedict.values())))
print(statedict)
