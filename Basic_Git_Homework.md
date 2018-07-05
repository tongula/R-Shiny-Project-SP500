## Basic Git Homework

Save all your commands (**not the output**) to *yourname.txt*. After you finish all the exercises, clone the homework repository from Github by typing the following command on the terminal:

`git clone https://github.com/casunlight/bootcamp014_homework.git`

Move your txt file to the `07-05BasicGit` folder under the repository you just cloned and then commit and push it.

**Note**: you may need to pull from the origin again before you push it to Github because your classmates might just pushed their homework.

Please go through this [online git tutorial](https://try.github.io/levels/1/challenges/1) to review what you have learned today.

### Question #1: 
1. Create a directory named `test` and make `test` a git repository.
2. Create a file named `a.txt` with the content “Git is good” under test and add `a.txt` to the staging area.
3. Run the command `git status` and check the output.
3. Now commit your changes and check the output of `git status`.

### Question #2:
1. Create a new directory named `subtest` under `test` repo and add a file `b.txt` into it, the content can be anything.
2. Add some more files, for example `a1.txt`, `a2.txt`, to test repo and then add all of them to the staging area.
3. Run `git status` to make sure you have everything in the staging area and then commit them.
4. Display all the commits we’ve done so far.

### Question #3:
1. Create a new repository with the name `test` under your Github account and follow the instruction on Github to link the remote repository with the local one you just created in question 1. This [instruction link](https://help.github.com/articles/adding-a-remote/) might be useful.
2. Add a new line “Github is awesome” to the file `a.txt` under the local repo `test` and then add the change to the staging area.
3. Now `a.txt` has two versions. Use `git diff` to show the difference between the old version that we just committed and the new one in the staging area.
4. Commit the changes and push it to the remote repo.

### Question #4:
1. Read [this tutorial](https://www.atlassian.com/git/tutorials/resetting-checking-out-and-reverting) and finish the following questions using the right commands.
2. Edit `a1.txt` and add it to the staging area and then try to undo the changes.
3. Edit the file `a1.txt` and commit it to the repository and then try to undo the changes you just committed.