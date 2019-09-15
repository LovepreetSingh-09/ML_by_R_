# Vectors :-
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)
temperature[1:2]
temperature[-2] # deletes the 2nd argument
temperature[-1] # deletes the 1st argument
temperature[c(flu_status)]

# Factors :-  Used for Categorical Variables
gender=factor(c('Male','Female','Male'))
gender
blood <- factor(c("O", "AB", "A"),levels = c("A", "B", "AB", "O"),ordered = TRUE)
blood<'AB'

# Lists :-
b=list(full_name=subject_name[1],temperature=temperature[1],flu=flu_status[1],gender=gender[1],blood=blood[1])
b
b[2]
b$temperature
b[[2]]
b[c('temperature','flu')]

# DataFrames:
data=data.frame(subject_name,temperature,flu_status,gender,blood)
data
data$temperature
data[1,3]
data[c(1,3),c(2,4)] # 1st and 3rd row of 2nd and 4th col
data[c('temperature','flu_status')]
data[1,]
data[c(1,3),c('temperature','blood')]
data[-2,c(-1,-3,-5)] # Erase 2nd row of all col and erase 1,3,5 col

