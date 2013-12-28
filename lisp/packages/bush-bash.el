;;; From: rms@gnu.ai.mit.edu (Richard Stallman)
;;; Subject: Bush-bash
;;; Date: 13 Sep 92 21:10:21 GMT
;;; Newsgroups: gnu.emacs.sources
;;; 
;;; Here is the bush-bashing program I promised.
;;; Simply load this, and each message you subsequently send
;;; with Emacs will have a randomly selected bush-bashing statement
;;; added at the end.  The statement is added in the buffer
;;; when you begin composing the message, so if you don't want it in
;;; a particular message, just delete it.

(defvar bush-list '(
"Family values means--if you want to get out of the draft, call your dad."

"People that are really very weird can get into sensitive positions and have
a tremendous impact on history.                -- Vice President Dan Quayle"

"Dan Quayle is a man of the future, a young man born in the middle of
 this century and from the middle of America.  He's a dynamic young
 leader for the future of our party and the future of the nation.
                -- President George Bush"

"Take out the word 'Quayle' and insert the word 'Bush' wherever it
 appears, and that's the crap I took for eight years.  Wimp.
 Sycophant.  Lap dog.  Poop.  Lightweight.  Boob.  Squirrel.  Asshole.
 George Bush.
                -- President George Bush"

"He's different from me.  I'm 64 and he's 41.  And that's good, that's
 positive.
                -- President George Bush, referring to Dan Quayle."

"[Dan Quayle is] my choice, my first choice, and my only choice.
                -- President George Bush"

"What a waste it is to lose one's mind, or not to have a mind is being
 very wasteful.  How true that is.
                -- Vice President Dan Quayle."

"I was not a very good student.  I was very average.
                -- Vice President Dan Quayle."

"Looking back, I should have pursued philosophy and history and
 economics and things of that sort in college more, but I didn't.
                -- Vice President Dan Quayle."

"The members of my generation who served in Vietnam made a sacrifice
 for their country that was far, far greater than mine.
                -- Vice President Dan Quayle."

"It was just a job.  It wasn't any special interest in consumer
 affairs.  I needed a paycheck and the Attorney General said that I
 would be best to go down there, because he knew I was anti-consumer.
                -- Vice President Dan Quayle, remembering his job as
                   chief investigator in the consumer products
                   division of the Indiana Attorney General's office."

"I don't know if she likes this, but in a way I treat her as a staff
 person.
                -- Vice President Dan Quayle, referring to Marilyn."

"There was never anything where I've got to work really hard to get
 there.
                -- Vice President Dan Quayle."

" I can identify with steelworkers. I can identify with workers that 
 have had a difficult time.
       -- Vice President Dan Quayle addressing workers at 
          an Ohio steel plant,1988"
 
"George Bush has the experience, and with me the future.
                -- Vice President Dan Quayle."

"Family is something which goes back to the nucleus of civilization.
 And the very beginnings of civilization, the very beginnings of this
 country, goes back to the family.
                -- Vice President Dan Quayle."

"I love California.  I grew up in Phoenix, Arizona.  A lot of people
 forget that.
                -- Vice President Dan Quayle."

"Even though Federal health officials are promoting less tobacco use,
 the tobacco industry should continue to expand in foreign markets.
                -- Vice President Dan Quayle."

"I was interested in joining the National Guard because it enabled me
 to go to law school as soon as possible.
                -- Vice President Dan Quayle."

"Somehow he got in [to law school]; he talked his way in.
                -- James Quayle, Dan's father."

"I deserve respect for the things I did not do.
                -- Vice President Dan Quayle."

"Dick [Cheney] and I have something in common.  That is that we both
 overmarried.
                -- Vice President Dan Quayle."

"My friends, we can and we will, never, never surrender to what is
 right.
                -- Vice President Dan Quayle, speaking to Pat
                   Robertson's Christian Coalition."

"I told him that I wouldn't even consider it unless they guaranteed me
 that I'd have no primary opposition and unless they would raise money
 for me.
                -- Dan Quayle, remembering being asked to run for
                   Congress."

"Hello everybody, I'm Dan Quayle.
                -- Robert Redford, having grown tired of asking Dan
                   Quayle to stop comparing himself to Redford."

"C'mon boys, don't bother me.  I'm debating Dan Quayle.  The boy's
 retarded.
                -- Senator Birch Bayh."

"The first year [1977] I spent getting my family moved to Washington.
 The second year I ran for re-election.  Then as soon as I was elected,
 I started running for the Senate.
                -- Vice President Dan Quayle, describing his career
                   in the House of Representatives."

"All the reporters ask me about my failures.  Are they kidding?  There
 are no failures.  Failure?  Failure isn't in my vocabulary.
                -- Vice President Dan Quayle."

"I stand by all the misstatements that I've made.
                -- Vice President Dan Quayle."

"One word sums up probably the responsibility of any vice president,
 and that one word is 'to be prepared'.
                -- Vice President Dan Quayle."

"I believe we are on an irreversible trend toward more freedom and
 democracy --- but that could change.
                -- Vice President Dan Quayle."

"[The United States will] work towards the elimination of human rights
 in El Salvador.
                -- Vice President Dan Quayle."

"I felt like I was in charge.
                -- Vice President Dan Quayle, after working the
                   Panama Canal's locks."

"And let me tell you, the people that I met, whether it was in Honduras
 or Jamaica, obviously, Panama, just people on the street, not one time
 did I get a negative response about the United States.  As a matter of
 fact, I didn't even get a, you know, a real thumbs down or the
 raspberries as you drove by, a better reception, quite frankly, in a
 couple of cities there than you might even get in --- here at home.
                -- Vice President Dan Quayle."

"You smile discreetly.  Look like you're enjoying yourself, like you're
 ready to get down to serious business.  You've got to be careful what
 you say.
                -- Vice President Dan Quayle, explaining to Latin
                   American leaders how to handle a photo op."

"I do have a political agenda.  It's to have as few regulations as
 possible.
                -- Vice President Dan Quayle."

"Mars is essentially in the same orbit... somewhat the same distance
 from the sun, which is very important.  We have seen pictures where
 there are canals, we believe, and water.  If there is water, that
 means there is oxygen.  If oxygen, that means we can breathe.
                -- Vice President Dan Quayle."

"As a matter of fact, I didn't understand it all.  Only thing I know is
that it looked good.
                -- Vice President Dan Quayle after being briefed
                   on Space Station Freedom by NASA."

"Quite frankly, teachers are the only profession that teach our
 children.
                -- Vice President Dan Quayle."

"We will move forward.  We will move upward and, yes, we will move
 onward.
                -- Vice President Dan Quayle."

"We're going to have the best-educated American people in the world.
                -- Vice President Dan Quayle."

"We are ready for any unforeseen event that may or may not occur.
                -- Vice President Dan Quayle."

"Vietnam is a jungle... Kuwait, Iraq, Saudi Arabia, you have sand.
                -- Vice President Dan Quayle."

"I'll tell you one person who doesn't think we've wasted our money on
 $600 toilet seats --- Saddam Hussein.
                -- Vice President Dan Quayle."

"In the past we have tried too much to prevent the making of mistakes.
                -- Vice President Dan Quayle."

"[The Gulf war was] a stirring victory for the forces of aggression
 against lawlessness.
                -- Vice President Dan Quayle."

"If we don't succeed, we run the risk of failure.
                -- Vice President Dan Quayle."

"I am now cashing in on being vice president for others.  They'll
 remember me.  I'll remember them.
                -- Vice President Dan Quayle."

"One thing we're able to do is raise money.
                -- Vice President Dan Quayle, on the GOP."

"You don't make money in politics.  Or, I should say --- you 
 shouldn't.
                -- Marilyn Quayle."

"I'm Dan Quayle.  I'm Dan Quayle.  I'm Dan Quayle.  I am Dan Quayle.
 The real Dan Quayle.  The real Dan Quayle stand up.  I'm Dan Quayle.
 I'm Dan Quayle.
                -- Vice President Dan Quayle, rehearsing with
                   media consultants."

"It's rural America.  It's where I come from.  We always refer to
 ourselves as real America.  Rural America, real America, real, real
 America.
                -- Vice President Dan Quayle."

"I am convinced as shoppers of Hy-Vee go, so goes the state of Iowa.
                -- Vice President Dan Quayle, speaking at a
                   Hy-Vee grocery store in Cedar Rapids, Iowa."

"I want to show you an optimistic sign that things are beginning to
 turn around.
                -- Vice President Dan Quayle, upon seeing a
                   Help Wanted sign in a California Burger King."

"You have a part-time job, you have a job.  That's better than no job
 at all.
                -- Vice President Dan Quayle, speaking on $4.25/hour
                   jobs with no benefits at Burger King."

"I'm Dan Quayle, who are you?
                -- Vice President Dan Quayle, greeting a woman at
                   a Hardee's restaurant with a handshake.  She
                   responded, ``I'm your Secret Service agent.''"

"As we were walking around in the store, Marilyn and I were just really
 impressed by all of the novelties and the different types of little
 things that you could get for Christmas.  And all the people that
 would help you, they were dressed up in things that said, ``I believe
 in Santa Clause.''  And the only thing that I could think is that I
 believe in George Bush.
                -- Vice President Dan Quayle."

"I try to stay away from liberalism.  I know how dangerous it is for
 our... Don't ask me to take a liberal position where I am right now.
 It's contrary to the administration's viewpoints.  It would not be
 terribly helpful to anyone, especially me.
                -- Vice President Dan Quayle."

"It is necessary to restate the President's viewpoint very clearly, and
 that is that we are a party that is diversified.  We are a party that,
 though we have a position on abortion, that those who disagree with us
 should not feel excluded because of that issue.  We do, in former
 Chairman Lee Atwater's words, offer the party as a big tent, and
 therefore that message has to be clear.  How we do that within the
 platform, the preamble to the platform or whatnot, remains to be seen,
 but the message will have to be articulated with great clarity.
                -- Vice President Dan Quayle."

"I deny that I have ever given my opinion to anybody about anything.
                -- President George Bush."

"I've learned the power of the word of a president.  Not maybe
 necessarily to make -- get something done, but the power of the word.
                -- President George Bush."

"I am less interested in what the definition is.  You might argue
 technically, are we in a recession or not.  But what there's this kind
 of sluggishness and concern -- definitions, heck with it.
                -- President George Bush."

"Please don't ask me to do that which I've just said I'm not going to
 do, because you're burning up time; the meter is running through the
 sand on you, and I am now filibustering.
                -- President George Bush, refusing to answer a
                   reporter's persistent questions about the Oliver
                   North trial."

"I've told you I don't live and die by the polls.  Thus I will refrain
 from pointing out that we're not doing too bad in those polls.
                -- President George Bush."

"It has been said by some cynic, maybe it was a former president, `If
 you want a friend in Washington, get a dog.'  We took them literally
 -- that advice -- as you know.  But I didn't need that, because I have
 Barbara Bush.
                -- President George Bush."

"I'm delighted that Barbara Bush is with me today, and I -- She got a
 good, clean bill of health yesterday from Walter Reed Hospital, I
 might add, and then -- But I'm taking another look at our doctor.  He
 told her it's okay to kiss the dog -- I mean -- no -- it's okay to
 kiss your husband, but don't kiss the dog.  So I don't know exactly
 what that means.
                -- President George Bush, who once said his wife
                   ``epitomizes a family value''."

"(1) I'm just not an emotional kind of guy.  (2) If I show some
 emotion, that's just the way I am.  (3) I'm not too good at the
 emotional side.  (4) We Bushes cry easily.  (5) If occasionally I do
 go up in smoke, it doesn't relate to this line of work.
                -- President George Bush on five separate occasions."

"High tech is potent, precise, and in the end, unbeatable.  The truth
 is, it reminds a lot of people of the way I pitch horseshoes.  Would
 you believe some of the people?  Would you believe our dog?  Look, I
 want to give the high-five symbol to high tech.
                -- President George Bush."

"Get this [economic plan] passed.  Later on, we can all debate it.
                -- President George Bush."

"I was elected.
                -- President George Bush, defending his '88 campaign
                   advertisements."

"[7/16/89] I've had no indication from home, nor have we picked up any
 here that they felt that the US economy was going to move towards a
 recession.  [10/4/91] The economy is moving in the right direction.
 [10/25/91] I don't want to buy into the predicate about [the US being
 in] another recession.  I don't feel that way.  [10/31/91] The
 economy's turned the corner, headed for recovery.  [11/8/91] I'm not
 prepared to say we are in a recession.  [1/4/92] It will not be a deep
 recession.  [1/15/92] Look, this economy is in free fall.
                -- President George Bush."

"When I need a little free advice about Saddam Hussein, I turn to
 country music.
                -- President George Bush."

"Boy, they were big on crematoriums, weren't they?
                -- Vice President George Bush at Auschwitz, 1987."

"It's no exaggeration to say the undecideds could go one way or
 another.
                -- President George Bush."

"All I was doing was appealing for an endorsement, not suggesting you
 endorse it.
                -- President George Bush, on his economic growth
                   proposal, speaking to Colorado Governor Roy Romer."

"Message: I care.
                -- President George Bush, in New Hampshire."

"I'm for Mr. Reagan -- blindly.
                -- Vice President George Bush, 1984."

"I will never apologize for the United States of America, ever.  I
 don't care what the facts are.
                -- President George Bush"

"I don't know whether I'd call it `cashing in'.  I expect every
 president has written his memoirs and received money for it.  Indeed,
 I read that a former president -- was it Grant?  Grant got half a
 million bucks -- that's when half a million really meant something.
                -- President George Bush, defending Ronald Reagan's $5
                   million autobiography deal."

"You can't argue with the vice president, can you?
                -- William Figueroa, the 12-year-old told by Dan
                   Quayle that potato is spelled 'potatoe'."

"This is a shocking and frightening pattern of investigations and
 intimidation.
                -- Bush spokesman Marlin Fitzwater, on Ross Perot's
                   investigations of Bush, which bear a striking
                   resemblance to the style of investigation the Bush
                   campaign has used on Dukakis and Clinton."

"The major cause of poverty is the birth of children to unwed mothers.
                -- Pat Robertson."

"I think it's pretty sad that someone can take $300 million and try to
 buy the presidency of the United States.
                -- Marilyn Quayle, on Ross Perot, who, as of the time
                   she said this (May 29), had spent less than $2
                   million, versus Bush and Clinton's $15m+ apiece."

"You can have a winner [in a nuclear war].
                -- George Bush, 1980.  In 1984, he said, ``I never
                   said that.''  The original interview had been taped."

"Amending the Constitution to protect the flag is not a matter of
 partisan politics.  It's an American issue.
                -- President George Bush, holding a miniature version
                   of the Iwo Jima memorial."

"We have a sluggish economy ... That's why I favor this deficit so
 much.
                -- President George Bush."

"I can't say I identify with any specific educational goal.
                -- George Bush, 1988."

"A kitchen in every pot.  I mean, a pot in every -- I mean, a chicken
 in every...
                -- George Bush, 1988."

"I plan to fish and hunt as much as I can.
                -- George Bush, on why he'd be a great environmental
                   president."

"The US government will make no concessions to terrorists.  It will not
 pay ransoms, release prisoners, change its policies or agree to other
 acts that might encourage additional terrorism.
                -- From the final report of the Vice President's Task
                   Force on Combatting Terrorism, 1986."

"We have engaged in a very -- a very -- an extraordinarily broad
 exercise of diplomacy here ... I don't know what -- what it means
 fully.
                -- President George Bush."

"I haven't been briefed [and] probably shouldn't comment.
                -- President George Bush, on East Germany's apology
                   for the Holocaust, April 1990."

"Wrestling with a marshmallow.
                -- An analyst from the conservative Heritage
                   Foundation, describing the process of reading
                   President George Bush's 32-page ``strategic
                   vision'' paper."

"We have given him everything we can.
                -- An aide to President George Bush, on assistance to
                   Gorbachev, prior to the Soviet coup."

"There is no Marshall.  And there is no plan.
                -- Sergey Plekhanov, Soviet scholar of the US, summer
                   1989."

"He is very sensitive to the reality that, in a sense, we could do too
 much.
                -- White House Chief of Staff John Sununu, on
                   President George Bush and the tiny amount of aid
                   offered Poland."

"We love your adherence to democratic principle, and to democratic
 processes.
                -- Vice President George Bush, toasting Ferdinand
                   Marcos, 1981.  He later said, ``I'll repeat it and
                   stand by it ... We should judge by the record.''"

"For somebody to suggest, as our two opponents have, that [the Marines
 killed in Beirut] died in shame...
                -- Vice President George Bush, at the 1984 debate.
                   When newspapers disproved this charge, his
                   strategist responded: ``So what?  Maybe 200 people
                   read it, or 2,000 or 20,000.''"

"The election is this: the Great Communicator against the Great
 Depressor... We are going for the gold!
                -- Vice President George Bush, 1984."

"When they talk about taxing the rich, they're really talking about
 taxing the working men and women of this country.
                -- President George Bush."

"A bunch of girly-men.
                -- Bush supporter Arnold Schwarzenegger, on the
                   Democratic presidential candidates, one of whom had
                   won the Medal of Honor.
  (I'm not a hero, but I play one on TV.)"

"And if I said a year ago that these [social] programs weren't working,
 perhaps I have been vindicated.
                -- President George Bush, on how Lyndon Johnson was to
                   blame for the LA riots."

"As you know, I planned a trip out there for some time, so it fits in
 very nicely.
                -- President George Bush, on his trip to LA after the
                   riots."

"Dukakis, September 1988: ``We'll... make the label MADE IN AMERICA the
 symbol of quality and durability all over the world.  
 Bush, January 1990: ``A better America, where MADE IN AMERICA is
 recognized around the world as a symbol of quality and progress."

"[Bush] so loves the Constitution that he overflows with ideas for
 improving it.
                -- Conservative columnist George Will."

"Bush so loves the flag he wraps himself in it, like Linus.
                -- Conservative columnist George Will."

"When you don't know where you're going, any gust of wind will get you
 there.
                -- Conservative columnist George Will, on George
                   Bush's foreign policy."

"Bush seems to be a bystander watching to see who Bush turns out to be.
                -- Conservative columnist George Will."

"George Bush says, `I am an environmentalist'.  That statement is as
 vacuous as any statement that can be constructed from four English
 words.
                -- Conservative columnist George Will."

"Bush skitters like a waterbug on the surface of things ... moving fast
 lest he linger so long that he is expected to show a mastery of, or
 even a real insterest in, anything.
                -- Conservative columnist George Will."

"He [Bush] is a borderline incompetent, if not incompetent.
                -- Head of the conservative Heritage Foundation, 1992."

"The difference is that Reagan had principles and beliefs.  [Bush] has
 no rudder.
                -- A senior GOP strategist, 1990."

"What we need is a good strategic thinker, and we don't have one.
                -- A White House official, 1989."

"[Bush] wants to know why can't he have initiatives to present to the
 public.
                -- A GOP strategist, 1992."

"The attempt to tear down our president's leadership with the knowledge
 of the issues has not failed.
                -- Vice President George Bush."

"Make no mistake about it, this president is in charge.  He is in
 touch.
                -- Vice President George Bush, on Ronald Reagan."

"If I had to do it over, I wouldn't do what I did then, for a lot of
 reasons, including political reasons.
                -- President George Bush, on raising taxes, March
                   1992.  On the campaign trail a few days later:
                   ``Life means nothing without fidelity to
                   principles.''"

"I have no idea what White House statement was issued, but I stand
 behind it 100 percent.
                -- Budget Director Richard Darman."

"[We're] running around like chickens with their heads cut off.
                -- An aide to President George Bush, March 1992."

"We finally get a policy-making body, but we have no policy to make.
                -- An aide to President George Bush, March 1992."

"He has a philosophy.  We just don't know what it is.
                -- An aide to President George Bush."

"What I said back then -- well, it's hard to find.  Number one, I
 didn't say it.
                -- George Bush, on calling Reagan's policies ``voodoo
                   economics'' during the 1980 primaries.  NBC
                   rebroadcast the tape."

"Keep playing with the same toys.  But let's paint them a little
 shinier.
                -- A domestic-policy adviser to President George Bush."

"Well, there's been dramatic progress.
                -- President George Bush, three weeks before death
                   squads killed six priests in El Salvador."

"We also need to assure that women don't have to worry about getting
 their jobs back after having a child.
                -- George Bush, campaigning in 1988.  Later, a
                   spokesman said that parents who cannot get unpaid
                   leave ``should look for other jobs''.
		   Now Bush opposes the legislation Congress has passed
		   to accomplish this, while blaming Congress for the 
	 	   inaction of his administration."

"I have no policy on that.
                -- President George Bush, on corporate takeovers and
                   mounting corporate debt, Jan 1990."

"From now on in America, `There's no room at the inn' -- that's simply
 not an acceptable answer.
                -- President George Bush, Christmas 1989."

"We'll figure it out.
                -- President George Bush, on how to pay for his $100
                   billion health-care plan, announced in 1992, which
                   involved giving poor people vouchers to buy private
                   health insurance at some point in the future when
                   it is cheaper, and which was never submitted
                   anyway."

"[I promise to] mount a comprehensive effort to reduce the cost of
 health care in America.
                -- George Bush, campaigning in 1988.  In February 1990
                   he said: ``The best prescription for better health
                   in America is a strong, daily dose of personal
                   responsibility.''"

"That's history.
                -- George Bush, on the 1988 campaign."

"Everything I did last year was for the purpose of advancing my --
 everything I did politically -- advancing my election.  And of course
 I'm not going to say that.
                -- President George Bush, June 1989."

"I know it sounds fishy, but it happens all the time.
                -- Neil Bush, George Bush's son, defending the
                   forgiveness of a $100,000 loan to Neil by
                   developers for whom Neil, on the board at
                   Silverado, had authorized $132 million in loans.
Is this what \"family values\" really means?"

"My position hasn't changed.  I am, uh, pro -- pro -- uh, prolife --
                -- President George Bush, April 1992.
I happen to think it was right.
                -- George Bush, March 1980, on his earlier support of
                   Roe vs Wade."

"Would I sell my services to a Third World country?  Ask again in six
 months.
                -- Chief scientist at a Russian nuclear facility,
                   after the Bush administration dithered on aid to Russia."

"It's way too early -- way too early -- to get into that.
                -- President George Bush, on aid to Russia after the
                   coup.  In November 1991, guards at a Soviet
                   nuclear-missile base left nuclear weapons unguarded
                   to forage for food."

"I tried to answer the question as vaguely as possible.
                -- President George Bush, asked about sending aid to
                   the Soviets in April 1991.  A few months later 
		   came the coup."

"Gorbachev has stolen our issues.
                -- Vice President Dan Quayle, when Gorbachev visited
                   China and espoused liberty, freedom and democracy."

"Public-relations gambits.
                -- A White House spokesman, when Gorbachev offered to
                   stop sending arms to the Sandinistas.
[It's part of] the public-relations battle.
                -- President George Bush, describing his
                   arms-reduction proposal.
Politics.
                -- Secretary of State James Baker's response when
                   Gorbachev withdrew 500 missiles from Europe."

"Looks pretty good to us.  It will not... affect our earnings.
                -- Exxon chairman Lawrence Rawl on the settlement he
                   reached with President George Bush's Justice
                   Department following the Exxon Valdez spill."

"So what you do is do the best you can, express the genuine concern you
 feel for the environment... but not take irresopnsible action to guard
 against incidents of this nature.
                -- President George Bush, commenting on the Exxon
                   Valdez oil spill for the first time, seven days
                   after it happened."

"I'm not sure whether there was any equivocation.
                -- President George Bush."

"3356.
                -- The year in which President George Bush's 1988
                   campaign pledge of 30,000,000 new jobs would be
                   fulfilled, at his current rate of job production
                   (as of July 1992)."

"Jobs, jobs, jobs.
                -- Secretary of State James Baker, on why we went to
                   war against Iraq.
Jobs, jobs, jobs.
                -- President Geroge Bush, on why he went to Japan in
                   1992.
30 in eight.
                -- George Bush's 1988 campaign pledge: 30 million new
                   jobs in eight years.  Currently (July 1992), he is
                   29,912,000 jobs short."

"People were out there looting their asses off... When they saw us,
 they shouted, `Viva Bush!'
                -- A US soldier present at the invasion of Panama."

"Bush had two basic messages for Noriega: We are aware of your
 unscrupulous activities, and those don't bother us much.  But you
 must... get firmly behind the contra effort.
                -- A colonel present at one meeting with George Bush
                   and Manual Noriega."

"I don't know what his problem with the Pledge of Allegiance is.
                -- George Bush, on Michael Dukakis, August 1988."

"Since we began restoring pride in the United States of America... flag
 sales have taken off.
                -- President George Bush."

"`And to the liberty for which it stands, one nation, under God, with
 freedom and justice for all.'  And let's never forget it.
                -- George Bush, mangling the Pledge of Allegiance."

"I'm opposed to these unsupervised weekend furloughs for first-degree
 murderers.
                -- George Bush, asked whether the Pledge of Allegiance
                   would be his only campaign issue in 1988."

"I have absolute confidence as to his integrity.
                -- George Bush, on Nixon, during Watergate."

"We aren't going to remake the world.
                -- President George Bush, after meeting Li Peng, the
                   butcher of Tiananmen."

"Using this fantastically, diabolically anti-me language... let them
 just stay tuned in.
                -- President George Bush, on his China-policy critics."

"In both our societies, there are voices of those who seek to redirect
 or frustrate our cooperation.  We must both take bold measures to
 overcome these negative forces.
                -- Brent Scowcroft, toasting the Chinese, December
                   1989 (six months after Tiananmen Square)."

"I don't think we ought to judge the whole People's Liberation Army by
 that terrible incident.
                -- President George Bush, four days after the
                   Tiananmen Square massacre."

"This is not the time for an emotional response.
                -- President George Bush, one day after the Tiananmen
                   Square massacre."

"I think perhaps this is a time for caution.
                -- President George Bush, during the pro-democracy
                   demonstrations in China."

"Fight for what you believe in, stand up for what you believe in.
                -- President George Bush, to Chinese dissidents, May
                   21 1989.  Nine days later he waived trade
                   restrictions against China."

"We [Reagan and Bush] have had triumphs, we have made mistakes, we have
 had sex.
                -- Vice President George Bush, May 1988."

"The deficit will be virtually eliminated by 1995.
                -- President George Bush, February 1991.  A year
                   later, his people estimated the deficit for fiscal
                   1992 at $399 billion."

"Little Weiner Countries.
                -- President George Bush's term for Third World
                   nations without oil."

"There will not be a murky ending.
                -- President George Bush, prior to invading Iraq."

"[Saddam Hussein is] Hitler revisited... worse than Hitler.
                -- President George Bush.  When challenged on the
                   analogy, he said: ``I didn't say the Holocaust.  I
                   mean, that is outrageous.''"

"Keep the war alive.
                -- A White House official, describing President Bush's
                   1992 reelection strategy."

"They're enormous!
                -- President George Bush, on the previous eight years'
                   deficits, Day 12 of the Bush presidency.
He didn't understand the deficit until after the election.
                -- An aide to President George Bush, Day 13 of the
                   Bush presidency."

"A lot is happening -- not all of it good, but a lot is happening.
                -- President George Bush, Day 48 of the Bush
                   presidency."

"Not one penny.
                -- President George Bush, on raising the $4.25-an-hour
                   minimum wage."

"White House officials worry that the coming evaluations of the `first
 hundred days' will suggest that the President... has no agenda, no
 money, no strategy, no ideology, no worldview, and no explanation for
 his mysterious role in the Iran-contra scandal.
                -- The New York Times, Day 85 of the Bush presidency."

"In the worst-case scenario, investigators would find a direct link to
 financing Iraqi military expenditures.
                -- President George Bush, in a memo to his secretary
                   of Agriculture, February 1990."

"I think the people want change.
                -- President George Bush, asked why he wants a second
                   term in office."

"I will do what I have to do to be reelected.
                -- President George Bush, January 1992."

"We'll stoop to whatever is necessary to win.
                -- A Bush (1992) campaign official."

"We're going to paint Clinton as a man out of control, who can't
 control his zipper, can't control his wife and can't control his
 waistline.
                -- A senior adviser to the 1992 Bush campaign."

"Just call him the vacation president.
                -- An aide to President George Bush."

"Fuck the Jews, they didn't vote for us anyway.
                -- Secretary of State James Baker."

"Wait and hope.
                -- A White House official, describing the president's
                   economic plan, November 1990.  On the backup plan:
                   ``There isn't one.''"

"While our economy may be beset by difficulty, it should not be beset
 by doubt.
                -- President George Bush, February 1991.
I am bullish on the economy.
                -- President George Bush, July 1991.
Although I believe that the economy is on the right track, let me be
 the first to say that all is not well.
                -- President George Bush, September 1991.
Job creation is fast.
                -- President George Bush, October 1991.
I've got to be careful I don't overcheerlead on this economy.
                -- President George Bush, November 1991.
I'm not basing any plans on the economy getting worse.
                -- President George Bush, November 1991."

"On the surface, selling arms to a country that sponsors terrorism, of
 course, clearly, you'd have to argue it's wrong, but it's the
 exception sometimes that proves the rule.
                -- Vice President George Bush, August 1987."

"Time's up!
                -- Vice President George Bush, to Alexander Haig,
                   when Haig asked questions about the Iran-contra
                   affair during a debate, January 1988."

"I sensed that we were sending arms.  And I sensed we were trying to
 get hostages out.  But not arms for hostages.
                -- George Bush."

"I know a lot -- close to it -- but I don't know whether I knew
 everything.
                -- George Bush, on the Iran-contra affair, January
                   1988."

"[May 1988] I was out of the loop.  [August 1988] I'm in on everything.
 If our policies aren't working, I can't say `Wait a minute, I'm not to
 blame' ... I feel I'm a full partner.
                -- George Bush, on Iran-contra."

"I bet I prepared a couple of hundred thousand pages of memoranda that
 went ... to the Vice President's office.
                -- Oliver North."
"Your dedication and tireless work on the hostage thing, with Central
 America, really gave me cause for great pride in you and thanks.  Get
 some turkey, George Bush.
                -- A thanksgiving letter to Oliver North, 1985."
"I support [the ERA].  Governor Reagan does not.  And I support
 Governor Reagan.
                -- George Bush."
"I do not want to amend the Constitution to override the decision of
 the Supreme Court [on abortion].
                -- George Bush, 1980.  On Day 5 of the Bush presidency
                   he proposed a right-to-life amendment."
"We seem to be zigzagging because sometimes it's less a matter of a
 game plan and more a matter of the president's moods.
                -- A White House official during the Persian Gulf
                   crisis."
"Saddam doesn't realize that if he doesn't get out, we're going to kick
 his ass out.
                -- President George Bush, January 1991."
"If I'm ever in a position to call the shots, I'm not going to rush to
 send somebody else's kids into a war.
                -- George Bush, _Man of Integrity_ (Harvest House,
                   1988).
If we let people see that kind of thing, there would never again be
 any war.
                -- A senior Pentagon official, explaining why they
                   refused to release video footage of fleeing Iraqi
                   soldiers being cut in half by helicopter cannon
                   fire, February 1991."
"Remember, when Hitler's war ended, there were the Nuremberg trials.
                -- President George Bush, October 1990.
You can count on it.
                -- President George Bush, on Iraqi war-crimes trials,
                   January 1991."
"Asking when [Saddam Hussein] will be overthrown is like asking when
 the economy's coming back.
                -- An aide to President Bush."
"[I will] fix [the S&L debacle] and fix it once and for all.
                -- President George Bush, June 1989.  As of March
                   1990, the S&L's were losing $3 million per hour."
"I'm very proud of my [environmental] record... I believe in fishing,
 believe in hunting, believe in camping.
                -- President George Bush."
"Our record alone won't cut it.
                -- White House Chief of Staff Sam Skinner to his
                   staff, on the importance of negative campaigning in
                   the 1992 election."

"Did you like Dukakis in a tank?  Then you must LOVE Bush in karate gear!"

"Dan Quayle's Top 10 TV complaints: 9. \"Sixty Minutes\" is on for a
whole hour."

"Dan Quayle's Top 10 TV complaints: 6. They keep showing the Rodney
King beatings over and over.  When are they going to get new episodes?"

"What a terrible thing it is to loose one's mind, or not to have
a mind as being very wasteful.  How true that is.
	-dan quayle, trying to paraphrase NAACP motto, \"A mind is
	 a terrible thing to waste.\""

"By the year 2000 we're going to have the best educated Americans
in the world.
	-dan quayle"

" Why wouldn't an enhanced deterrent, a more stable peace, a better
 prospect to denying the ones who enter conflict in the first place
 to have a reduction of offensive systems and an introduction to
 defensive capability.  I believe that is the route this country
 will eventually go.
       -- Vice President Dan Quayle"
 

" Republicans understand the importance of bondage between a mother and
child.
       -- Vice President Dan Quayle"
 

"Mars is essentially in the same orbit... somewhat the same distance
from the Sun, which is very important. We have seen pictures where
there are canals, we believe, and water. If there is water, that means
there is oxygen. If oxygen, that means we can breathe.
       -- Vice President Dan Quayle"
 

" Hawaii has always been a very pivotal role in the Pacific.  It is IN
 the Pacific.  It is a part of the United States that is an island that 
 is right here.
       -- Vice President Dan Quayle,
          Hawaii, September 1989"
 

" What a terrible thing to have lost one's mind.  Or not to have a mind
 at all.  How true that is.
       -- Vice President Dan Quayle winning friends while
          speaking to the United Negro College Fund"
 

" You all look like happy campers to me.  Happy campers you are, happy
 campers you have been, and, as far as I am concerned, happy campers you
 will always be.
       -- Vice President Dan Quayle, to the American Samoans,
          whose capital Quayle pronounces 'Pogo Pogo'"
 

" 'The Holocaust was an obscene period in our nation's history. I mean
 in this century's history. But we all lived in this century. I didn't
 live in this century.' 
       -- Vice President Dan Quayle 
          (The New Yorker, October 10, 1988, p.102)"
 

" We expect them [Salvadoran officials] to work toward the elimination
 of human rights.
       -- Vice President Dan Quayle"
 

"El Salvador is a democracy so it's not surprising that there are many
voices to be heard here. Yet in my conversations with Salvadorans... I
have heard a single voice.
       -- Vice President Dan Quayle"
 

" I believe we are on an irreversible trend toward more freedom and
 democracy - but that could change.
       -- Vice President Dan Quayle"
 

" If we do not succeed, then we run the risk of failure.
       -- Vice President Dan Quayle, to the Phoenix Republican
          Forum, March 1990"
 

"It's rural America.  It's where I came from.  We always refer to
ourselves as real America.  Rural America, real America, real, real,
America.
       -- Vice President Dan Quayle"
 

" Target prices?  How that works?  I know quite a bit about farm policy.
 I come from Indiana, which is a farm state.  Deficiency payments -
 which are the key - that is what gets money into the farmer's hands.
 We got loan, uh, rates, we got target, uh, prices, uh, I have worked
 very closely with my senior colleague, (Indiana Sen.) Richard Lugar,
 making sure that the farmers of Indiana are taken care of.
       -- Vice President Dan Quayle on being asked to
          define the term 'target prices.'
          Quayle's press secretary then cut short the press 
          conference, after two minutes and 30 seconds."
 

" I'm not going to focus on what I have done in the past
 what I stand for, what I articulate to the American people.
 The American people will judge me on what I am saying and what I 
 have done in the last 12 years in the Congress.
       -- Vice President Dan Quayle"
 

"We should develop anti-satellite weapons because we could not have
prevailed without them in 'Red Storm Rising'.
       -- Vice President Dan Quayle"
 

" The US has a vital interest in that area of the country.
       -- Vice President Dan Quayle Referring to Latin America."
 

" Japan is an important ally of ours. Japan and the United States of 
 the Western industrialized capacity, 60 percent of the GNP,
 two countries. That's a statement in and of itself.
       -- Vice President Dan Quayle"
 

"Who would have predicted... that Dubcek, who brought the tanks in in
Czechoslovakia in 1968 is now being proclaimed a hero in
Czechoslovakia.  Unbelievable.
       -- Vice President Dan Quayle
          Actually, Dubcek was the leader of the Prague Spring."
 

"Certainly, I know what to do, and when I am Vice President -- and I
will be -- there will be contingency plans under different sets of
situations and I tell you what, I'm not going to go out and hold a
news conference about it. I'm going to put it in a safe and keep it
there!  Does that answer your question?
       -- Vice President Dan Quayle when asked what he 
          would do if he assumed the Presidency (1988)"
 

"Lookit, I've done it their way this far and now it's my turn. I'm my
own handler. Any questions? Ask me ... There's not going to be any
more handler stories because I'm the handler ... I'm Doctor Spin.
       -- Vice President Dan Quayle responding to press reports
          his aides having to, in effect, 'potty train' him."
 

" I would guess that there's adequate low-income housing in this
 country.
       -- Vice President Dan Quayle"
 

" Verbosity leads to unclear, inarticulate things.
       -- Vice President Dan Quayle"
 

" Let me just tell you how thrilling it really is, and how, 
 what a challenge it is, because in 1988 the question is 
 whether we're going forward to tomorrow or whether we're 
 going to go past to the -- to the back!
       -- Vice President Dan Quayle"
 

" We don't want to go back to tomorrow, we want to go forward.
       -- Vice President Dan Quayle"
 

" I have made good judgments in the Past. 
 I have made good judgments in the Future. 
       -- Vice President Dan Quayle"
 

" The future will be better tomorrow.
       -- Vice President Dan Quayle"
 

"I have a very strong record on the Environment in the United States
Senate.
       -- Vice President Dan Quayle"
 

" We will invest in our people, quality education, job opportunity,
 family, neighborhood, and yes, a thing we call America.
       -- Vice President Dan Quayle, 1988"
 

" We'll let the sunshine in and shine on us, because today we're
 happy and tomorrow we'll be even happier.
       -- Vice President Dan Quayle, 1988"
 

" We're going to have the best-educated American people in the
 world.
       -- Vice President Dan Quayle"
 

" This election is about who's going to be the next President of the
 United States!
       -- Vice President Dan Quayle, 1988"
 

"During the White House Easter Egg Roll of 1991, Quayle signed
autographs using only his finger. He had prepared pre-signed cards
which his aides handed out while he made signing gestures. This
allowed him to move briskly and efficiently through the crowd, said
his spokesman."
 

" Dan Quayle, in April 1991, was concerned that his advisors
 may be getting out of touch with 'Real Americans.' In order
 to combat this, he suggested that they read People magazine.
 People that are really very weird can get into sensitive positions
 and have a tremendous impact on history.
       -- Vice President Dan Quayle"
 

"I'm going to be a vice president very much like George Bush was.  He
proved to be a very effective vice president, perhaps the most
effective we've had in a couple of hundred years.
       -- Vice President Dan Quayle"
 

" The loss of life will be irreplaceable.
       -- Vice President Dan Quayle 
          after the San Francisco earthquake"
 

" I couldn't help but be impressed by the magnitude of the earthquake.
       -- Vice President Dan Quayle, stepping out of
          the helicopter upon arrival at Alameda Naval Air
          Station."
 

"Let me tell you something. As we were walking around in the store,
Marilyn and I were just really impressed by all the novelties and the
different types of little things that you could get for Christmas. And
all the people that would help you, they were dressed up in things
that said 'I believe in Santa Claus.' And the only thing that I could
think is that I believe in George Bush.
       -- Vice President Dan Quayle at a garden center and 
          produce store in Baltimore (from the Los Angeles Times, 
          Douglas Jehl, November 6, 1988)"
 

"It's a very valuable function and requirement that you're performing,
so have a great day and keep a stiff upper lip.
       -- Vice President Dan Quayle
          remarks to oil spill clean-up workers at Prince 
          William Sound, May, 1989"
 

" The President is going to benefit from me reporting directly to him
 when I arrive.
       -- Vice President Dan Quayle
          remarks to oil spill clean-up workers at Prince 
          William Sound, May, 1989"
 

" It isn't pollution that's harming the environment.  It's the
 impurities in our air and water that are doing it.
       -- Vice President Dan Quayle"
 

" We have a firm commitment to NATO, we are a *part* of NATO. We 
 have a firm commitment to Europe. We are a *part* of Europe.
       -- Vice President Dan Quayle"
 

"I could take this home, Marilyn. This is something teenage boys might
find of interest.
       --Vice President Dan Quayle, when purchasing a South 
         African Indian Doll that, when lifted, displays an erection."
 

" Public Speaking is very easy.
       -- Vice President Dan Quayle to reporters in 10/88"
 

"The other day [the President] said, I know you've had some rough
times, and I want to do something that will show the nation what faith
that I have in you, in your maturity and sense of responsibility. (He
paused, then said) Would you like a puppy?
       -- Vice President Dan Quayle (LA Times 5/21/89)"
 

" In George Bush you get experience, and with me you get- The Future!
       -- Vice President Dan Quayle in eastern Illinois 
         (LA Times 10/19/88)"
 

" The destruction, it is just very heart-rendering.
       -- Vice President Dan Quayle attempting to say the 
         SF earthquake wreckage was heart-rending     
         (Newsweek 10/30/89)"
 

" I spend a great deal of time with the President. We have a very 
 close, personal,loyal relationship. I'm not, as they say, a potted 
 plant in these meetings.
       -- Vice President Dan Quayle defending himself 
         (Tampa Tribune-Times  1/7/90)"
 

" I'm glad you asked me that. This gives me the perfect
 opportunity to talk about the problems with this Congress...
       -- Vice President Dan Quayle responding to reporter's
          questions about his use of Air force 2 to
          go on golf trips at the cost of $26,000/hour"
 

" I love California; I practically grew up in Phoenix.
       -- Vice President Dan Quayle"
 

" My friends, no matter how rough the road may be, we can and we will,
 never, never surrender to what is right
       -- Vice President  Dan Quayle, in a speech 
          to the Christian Coalition"
 

" Are they taking DDT?
       -- Vice President Dan Quayle asking doctors at a Manhattan
          AIDS clinic about their treatments of choice.
          (NY Post, early May 92)"
 


" I just don't believe in the basic concept that someone should make their
 whole career in public service.
       -- Vice President Dan Quayle"
 

" If you listen to the news, read the news, you'd think we were still
 in a recession. Well, we're not in a recession. We've had growth;
 people need to know that. They need to be more upbeat, more positive...
       -- Vice President Dan Quayle in October 91
 Need any help? 
       -- Vice President Dan Quayle in October 91 addressing 
          announced 74,000 layoffs"
 

"The message of David Duke, is this, basically: Big government,
anti-big government, get out of my pocketbook, cut my taxes, put
welfare people back to work. That's a very popular message. The
problem is the messenger.
       -- Vice President Dan Quayle "
 

" Who's responsible for the riots? The rioters!
       -- Vice President Dan Quayle giving an intelligent, in-depth 
          analysis of the LA riots. (Herb Caen, SF Chronicle)"
 

"It's immoral to parent irresponsibly... And it doesn't help matters
any when prime time TV, like 'Murphy Brown', a character who is
supposed to represent a successful career woman of today, mocks the
importance of the father by bearing a child alone, and calling it just
another 'lifestyle choice.' Marriage is probably the best anti-poverty
program there is...  Even though our cultural leaders in Hollywood,
network TV, the national newspapers routinely jeer at [such values] I
think most of us in this room know that some things are good, and
other things are wrong.
       -- Vice President Dan Quayle addressing the 
          Commonwealth Club of San Francisco and criticizing
          Murphy Brown's decision to NOT have an abortion
          and to be a single (highly successful) mother."
 

"When told about Quayle's comments on Murphy Brown, a senior Bush
campaign official replied only 'Oh, dear.'  Bush's top aid said, 'The
world is a lot more complex than Dan would like to believe'."
 

" I think especially in her position, a highly successful professional 
 woman, it would be a real exception to have an unwed child. 
       -- Vice President Dan Quayle to The Chron's Jerry Roberts."
 

" I don't watch it, but I know enough to comment on it.
       -- Vice President Dan Quayle defending his opinions about
          the TV show 'Murphy Brown' [Las Vegas RJ 21 May 92]"
 

" The intergenerational poverty that troubles us so much today is 
 predominantly a poverty of values.
       -- Vice President Dan Quayle"
 

" Speaking as a man, it's not a woman's issue.  Us men are tired 
 of losing our women
       -- Vice President Dan Quayle talking about
          breast cancer"
 

" I want to show you an optimistic sign that things are beginning 
 to turn around.
       -- Vice President Dan Quayle trying to convince reporters 
          that the economy was doing better because a 
          Burger King had a 'now hiring' sign in the window.
          He was campaigning for reelection in Ontario, CA
          in January 1992."

" You have a part-time job and that's better to no job at all
       -- Vice President Dan Quayle after the manager of the 
          Burger King had said that the jobs offered were part-time 
          minimum wage jobs, which didn't pay enough to live on, 
          and that 'It's hard to find people who want to actually 
          show up for the job.'"
 

" We're in Florida.
       --  Vice President Dan Quayle explaining why he
           had just purchased four peaches (and no citrus 
           fruits -- for which Florida is famous) at a Public 
           supermarket in Oakland Park, Florida. Georgia (which 
           IS famous for peaches) did not gain from the transaction, 
           however; the peaches were from Chile. (The Sunstenial)"
 

" I feel that this [1981] is my first year, that next year is an
 election year, that the third year is the mid point and that the
 fourth year is the last chance I'll have to make a record since the
 last two years, I'll be a candidate again.  Everything I do in those
 last two years will be posturing for the election.  But right now I
 don't have to do that.
       -- Senator Dan Quayle "
 

"My position is that I understand from a medical situation, immediately
after a rape is reported, that a woman normally, in fact, can go to
the hospital and have a D and C.  At that time...  that is before the
forming of a life.  That is not anything to do with abortion
       -- Vice President Dan Quayle  explaining that Dilatation
          and Curettage, a form of abortion which occurs 
          after fertilization, is not really abortion.
          (the Washington post, 11/03/88)"
 

" Add one little bit on the end...  Think of 'potato,' how's it spelled?  
 You're right phonetically, but what else...?  There ya go...alright!
       -- Vice President Dan Quayle correcting a student's
          correct spelling of the word 'potatoe'  during
          a spelling bee at an elementary school in Trenton."
 

" I should have caught the mistake on that spelling bee card.  But 
 as Mark Twain once said, 'You should never trust a man who has only 
 one way to spell a word.'
       -- Vice President Dan Quayle, actually quoting from 
          President Andrew Jackson."
 

" People who Bowl Vote.
 Bowlers are not the cultural elite.
       -- Vice President Dan Quayle while at a Las Vegas bowling 
          alley. the Vice-President bowled 5 times, and knocked 
          down 19 pins.  (6/25/92, San Jose Mercury News)
          The American Bowling Congress projected his score for a 
          full game to be 76.  The Detroit average for amateur 
          players is 163 (USA Today, 7/6/92)"
 

" Dan Quayle had a trip planned to Beijing, but was worried because of
 the turmoil at that end. His security adviser however informed him that
 it was pretty safe for D.Q. as, 'They are only harassing intellectuals.'
 And the President put his hand on my shoulder and said: 'Dan, 
 I _knew_ Spiro Agnew.  He was a friend of mine.  And Dan...  
 You're no Spiro Agnew!'
       -- Vice President Dan Quayle "
 

" This president is going to lead us out of this recovery.
       -- Vice President Dan Quayle at a campaign stop in 
          California and and then at CA State University, Fresno
          (The Quayle Quarterly, Spring/Summer 1992)"
 

" We have to do more than just elect a new president if we truly want to   
 change this country.
       -- Vice President Dan Quayle         "
 

" We are ready for any unforeseen event that may or may not occur
       -- Vice President Dan Quayle, September 1990  "
 

" For NASA, space is still a high priority.
       -- Vice President Dan Quayle, September 1990  "
 

" [The U.S. victory in Gulf war was a] stirring victory for the 
 forces of aggression.
       -- Vice President Dan Quayle,  April 1991"
 

" I hope I never have to deal with it. But obviously I would counsel her 
 and talk to her and support her on whatever decision she made.
       -- Vice President Dan Quayle responding to Larry
          King's question of how he would react if his 13-year-old
          daughter chose to have an abortion. (CNN, July 22, 1992)
          Marilyn Quayle later remarked that her daughter would 
          'take the child to term.'"
 

"No, I had no problem communicating with Latin American heads of state -
though now I do wish I had paid more attention to Latin when I was in
high school.
		-- Vice President Dan Quayle"


"The loss of life will be irreplaceable.
                -- Vice President Dan Quayle
                   after the San Francisco earthquake"

"The cause of the riots were the rioters
                -- Vice President Dan Quayle giving an intelligent
                   analysis of the LA riots."


"Dan Quayle says: Don't Forget to Vot"


"Since Bush became president...
National debt:  +$1,477,000,000,000 (up 57%)"

"Since Bush became president...
Budget deficit:  +$244,500,000,000 (up 157%)"

"Since Bush became president...
Americans unemployed for 6 months or longer:  +1,127,000 (up 133%)"

"Since Bush became president...
Mean annual income of the wealthiest 5% of US families:  +$7,286 (up 5%)
Mean annual income of the poorest one-fifth:  -$54 (down .5%)"

"Since Bush became president...
Hourly earnings for blue-collar workers:  -$1 (down 10%)"

"Since Bush became president...
Number of millionaires:  +28,100 (up 81%)
Americans with no health insurance:  +4,600,000 (up 15%)"

"Since Bush became president...
Families receiving Aid to Families with Dependent Children:  +944,000 (up 25%)
Median monthly payment to those families:  -$49.26 (down 12%)"

"Since Bush became president...
Children living in poverty:  +500,000 (up 4%)"

"Since Bush became president...
Americans in prison:  +177,122 (up 28%)
Americans on death row:  +464 (up 22%)
Violent crimes:  +95,278 (up 21%)"

"I used to smoke marijuana, but that was when I was in college, before it
was a drug.
        --J. Danforth Quayle"
))

(defvar bash-bush-last-idx 0)

(defun bash-bush ()
  (interactive)
  (let* ((len (length bush-list)) ran)
    (setq ran (abs (random)))
    (setq bash-bush-last-idx
	  (% (+ bash-bush-last-idx (% ran len)) len))
    (goto-char (point-max))
    (insert "\n\n" (elt bush-list bash-bush-last-idx) "\n")))

(add-hook 'mail-setup-hook 'bash-bush)
