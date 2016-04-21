## -*- coding: utf-8 -*-

MAX.REACTION.TIME = 3000
PRE.FIXATION.TIME.MIN = 1000
PRE.FIXATION.TIME.MAX = 2000
FIXATION.TIME = 1000
POST.FIXATION.TIME = 100

## Obiekty graficzne

RECT = new(RectangleShape, WINDOW$get.size())
RECT$set.scale(c(.15, .25))
center(RECT, WINDOW)
RECT$set.outline.thickness(WINDOW$get.size()[1] * .01)
TXT$set.string("Proszę nacisnąć spację")
center(TXT, WINDOW)
FX = fixation(WINDOW)
CUE = new(ConvexShape, 3)
CUE$set.point(0, c(0, 0))
CUE$set.point(1, c(.5, 2))
CUE$set.point(2, c(-.5, 2))
CUE$set.scale(WINDOW$get.size() * .02)
center(CUE, WINDOW)

KEYS <<- c(Key.Left, Key.Right)

draw.boxes = function(){
    RECT$set.fill.color(c(0, 0, 0))
    RECT$set.position(WINDOW$get.size() * c(.25, .5))
    WINDOW$draw(RECT)
    RECT$set.position(WINDOW$get.size() * c(.75, .5))
    WINDOW$draw(RECT)
}

draw.stim = function(side){
    RECT$set.fill.color(c(0, 1, 0))
    RECT$set.position(WINDOW$get.size() * c(c(left = .25, right = .75)[side], .5))
    WINDOW$draw(RECT)
}

draw.cue = function(side){
    CUE$set.rotation(c(left = -90, right = 90)[side])
    WINDOW$draw(CUE)
}

trial.code = function(trial, side = sample(c('left', 'right'), 1),
                      cue = sample(c(0, 1), 1),
                      valid = sample(c(0, 1, 1, 1), 1)){
    ## Kod specyficzny dla zadania
    pre.fixation.time = runif(1, PRE.FIXATION.TIME.MIN, PRE.FIXATION.TIME.MAX)
    ## Szablon
    if(trial == 1){
        state = 'press-space'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                WINDOW$clear(c(0, 0, 0))
                draw.boxes()
                WINDOW$display()
                start = CLOCK$time
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            if((CLOCK$time - start) > pre.fixation.time){
                WINDOW$clear(c(0, 0, 0))
                draw.boxes()
                if(cue){
                    draw.cue(ifelse(valid, side, setdiff(c('left', 'right'), side)))
                }else{
                    lapply(FX, WINDOW$draw)
                }
                WINDOW$display()
                fixation.start = CLOCK$time
                state = 'clear-fixation'
            }
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                draw.boxes()
                WINDOW$display()
                fixation.cleared = CLOCK$time
                state = 'post-fixation'
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME)state = 'show-stim'
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            draw.boxes()
            draw.stim(side)
            WINDOW$display()
            stim.onset = CLOCK$time
            CORRECT.KEY <<- c(left = Key.Left, right = Key.Right)[side]
            ACC <<- RT <<- NULL
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(!is.null(ACC) || ((CLOCK$time - stim.onset) > MAX.REACTION.TIME))state = 'done'
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            draw.boxes()
            WINDOW$display()
            return(list(soa = floor(pre.fixation.time), rt = ifelse(is.null(RT), MAX.REACTION.TIME, RT - stim.onset),
                        acc = ifelse(is.null(ACC), 2, ACC)))
        })
    }
}

gui.show.instruction("Za chwilę pojawi się okno danych osobowych.

Jako identyfikator należy podać pierwsze dwa inicjały, dzień i miesiąc urodzenia.

Na przykład, Jan Kowalski urodzony 11 grudnia będzie miał identyfikator jk1112
")

if(!interactive())gui.user.data()

gui.show.instruction("To zadanie polega na reagowaniu za pomocą klawiszy strzałek na pojawiające się\
zielone prostokąty. Przez cały czas trwania zadania widoczne są dwie białe ramki, jedna po lewej,\
a jedna po prawej stronie od środka ekranu. Na początku próby na środku między nimi pojawia się strzałka.\
Po pewnym czasie od rozpoczęcia próby w środku jednej z ramek pojawia się zielony prostokąt.

Jeżeli zielony prostokąt pojawił się po lewej stronie, należy szybko nacisnąć klawisz strzałka w lewo.

Jeżeli zielony prostokąt pojawił się po prawej stronie, należy szybko nacisnąć klawisz strzałka w prawo.

W trakcie badania proszę zachować ciszę.

Należy reagować możliwie szybko i poprawnie.")

if(!interactive()){
    ## 16 warunków
    run.trials(trial.code, expand.grid(side = c('left', 'right'),
                                       cue = c(1, 1),
                                       valid = c(0, 1, 1, 1)),
               b = 2, n = 20, max.time = 15 * 60 * 1000)
    quit("no")
}else{
USER.DATA = list(name = 'admin', age = 37, gender = 'M')
    run.trials(trial.code, expand.grid(side = c('left', 'right'),
                                       cue = c(0, 1),
                                       valid = c(0, 1, 1, 1)), b = 1, n = 1)
}
