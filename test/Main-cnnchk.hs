--
-- CNN check
--

module Main (
  main
) where

import Control.Monad
import Data.Time
import Debug.Trace

import CNN.Algebra
import CNN.Image
import CNN.LayerType
import CNN.Layer
import CNN.ActLayer
import CNN.PoolLayer
import CNN.ConvLayer
import CNN.FullConnLayer

import Pool
import Trainer

x :: [Trainer]
x = [([[[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.0,0.5,0.5,0.5],
        [0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],
        [0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],
        [0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5],
        [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.5,0.0],
        [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.0,0.0,0.0,0.0],
        [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
        [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.0,0.0],
        [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.0,0.0,0.0],
        [0.0,0.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
        [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
        [0.0,0.0,0.0,0.0,0.5,0.0,0.0,0.0,0.0,0.0,0.5,0.0]]],
        [1.0,0.0,0.0])]

fc1 :: [FilterC]
fc1 = [([[ 0.085905  , -0.0678689 ,  0.3814169 ,
           0.43463653,  0.04209257,  0.20257941,
          -0.41531846,  0.31991767,  0.43873657]],0.0),
       ([[-0.15370686,  0.30818845,  0.09809999,
          -0.05181358,  0.02393685,  0.24620931,
          -0.21878266, -0.0326504 ,  0.30548354]],0.0),
       ([[ 0.15663988, -0.11626568, -0.31853248,
          -0.08551628,  0.37949085,  0.15236294,
          -0.07106829,  0.06041856, -0.40324084]],0.0),
       ([[ 0.43609989,  0.10470975,  0.2458974 ,
           0.3795274 , -0.13935709,  0.27390678,
           0.3279409 ,  0.14605017,  0.07812175]],0.0),
       ([[ 0.34610384, -0.04524003, -0.22491246,
           0.27643535,  0.17346834, -0.0795374 ,
          -0.43491274,  0.39947938,  0.09260994]],0.0),
       ([[-0.43418139, -0.00132555,  0.08678753,
          -0.1758468 , -0.03364892, -0.00387168,
           0.07537   ,  0.32523807,  0.29232075]],0.0),
       ([[ 0.2451028 ,  0.04261545,  0.09982441,
           0.20612988,  0.05839701,  0.24215263,
           0.42810306,  0.25446708,  0.10060225]],0.0),
       ([[-0.30066952, -0.02077727, -0.19036335,
          -0.3046046 ,  0.24392145, -0.3880607 ,
          -0.37032283,  0.14451058,  0.02054084]],0.0),
       ([[-0.04715565, -0.16996655, -0.42473247,
          -0.18167216, -0.20431407, -0.40762712,
           0.26751916, -0.14623689,  0.34481431]],0.0),
       ([[ 0.27307444,  0.40835462,  0.40273468,
           0.08021495, -0.16585497,  0.36816116,
          -0.39537489, -0.24131427, -0.12080253]],0.0)]

fc2 :: [FilterC]
fc2 = [([[-0.05549437, 0.2812032 , 0.05020593,-0.19811028],
         [ 0.06102226, 0.27433157,-0.23139136, 0.30502289],
         [-0.21890826, 0.20284628, 0.11228393,-0.06065928],
         [-0.17880337, 0.17303018, 0.21990221, 0.01878369],
         [-0.05717416,-0.3046488 ,-0.22220717, 0.19247847],
         [-0.05073677,-0.10168774,-0.12829623, 0.13786645],
         [ 0.31484091, 0.03282588, 0.24659191,-0.20967907],
         [ 0.24258736, 0.12824657,-0.04926281, 0.22978055],
         [-0.16757338, 0.04031946,-0.29942959,-0.07586212],
         [-0.08738316,-0.25852029,-0.28760692,-0.09964241]],0.0),
       ([[-0.09144423, 0.31359564, 0.28093964,-0.23328495],
         [ 0.01160876,-0.01439129,-0.0625458 , 0.27234446],
         [ 0.00969943, 0.19579686, 0.24145396, 0.16612569],
         [-0.02250232,-0.2694769 ,-0.01865784,-0.18937491],
         [-0.18431827,-0.20394186,-0.0921858 , 0.26041554],
         [-0.04204506,-0.20346355, 0.15948141,-0.28430738],
         [-0.2639436 ,-0.29182545,-0.02195974, 0.06566701],
         [-0.20125409, 0.01203544, 0.1329126 , 0.26878119],
         [ 0.01842447, 0.29800402, 0.02988527,-0.31215603],
         [-0.0172175 , 0.01396973,-0.2199624 , 0.0975462 ]],0.0),
       ([[-0.23372797,-0.0289988 , 0.30507217,-0.23241849],
         [-0.27810403,-0.27330157,-0.25541967, 0.20196985],
         [ 0.08197035, 0.179384  ,-0.26744305, 0.29851794],
         [ 0.2421252 , 0.08249315, 0.27136965,-0.06443039],
         [-0.00700375, 0.11751228,-0.13813045, 0.11175486],
         [-0.0026995 , 0.18413436,-0.17782529, 0.15570792],
         [-0.02902272,-0.05802   , 0.26730321,-0.22090366],
         [-0.23338694,-0.07469927, 0.02655466, 0.28104436],
         [-0.11178097,-0.25460005, 0.28295255, 0.22580865],
         [-0.29976061,-0.24166525, 0.20831591,-0.22864893]],0.0),
       ([[ 0.0439374 ,-0.01405606,-0.12226221,-0.28042631],
         [ 0.27627884, 0.25551431, 0.14055402, 0.0974476 ],
         [-0.06817242, 0.29957824, 0.13345293,-0.0043966 ],
         [-0.10976804,-0.24295586, 0.2985574 ,-0.14266408],
         [-0.19767417,-0.18031615, 0.16497855, 0.28137271],
         [ 0.00098066, 0.10251067,-0.09651004,-0.23491296],
         [ 0.11466183,-0.06875847,-0.2996546 , 0.00037928],
         [ 0.01778915,-0.17502066,-0.31515912,-0.03379622],
         [ 0.00336834,-0.26498695, 0.20411864,-0.16431515],
         [-0.3100991 ,-0.17389888,-0.26298125, 0.20615811]],0.0),
       ([[ 0.13583072,-0.16155688,-0.2758538 ,-0.04632471],
         [ 0.1303121 , 0.13797244,-0.19388915, 0.15181975],
         [ 0.28886134, 0.01005223,-0.04424328,-0.30165804],
         [ 0.00463335, 0.11799231,-0.22883806, 0.10904979],
         [-0.29136501,-0.04314832, 0.22406372,-0.04912378],
         [-0.26028764,-0.28037069,-0.25091228,-0.00928717],
         [ 0.11046289,-0.25874082, 0.21010951, 0.17485089],
         [-0.27754466,-0.11231281,-0.07770368, 0.00143252],
         [-0.03164469,-0.02064208,-0.25000056,-0.079847  ],
         [-0.30245778,-0.03267406, 0.24862764, 0.17448261]],0.0),
       ([[-0.15864431, 0.14644161,-0.11691701,-0.00244731],
         [-0.28433566, 0.05219023,-0.13749962,-0.13070581],
         [-0.23650416, 0.24866945,-0.03081761, 0.09541946],
         [ 0.19562176,-0.13726634,-0.14332594,-0.07226615],
         [-0.04957938, 0.27332542, 0.23251141,-0.03467474],
         [-0.29776676,-0.24842964,-0.19210258,-0.16555387],
         [ 0.20523843, 0.2507595 ,-0.29859035, 0.04635983],
         [ 0.19330568,-0.05027235, 0.01598015,-0.1352553 ],
         [ 0.10033669,-0.25911677, 0.20381848, 0.19442587],
         [-0.19503874, 0.08747051, 0.21026909,-0.01908302]],0.0),
       ([[ 0.05081972,-0.2473516 ,-0.023768  , 0.24435035],
         [-0.02214684,-0.02136734,-0.19226353,-0.29173347],
         [ 0.07327741, 0.04087641,-0.24777659, 0.15282225],
         [-0.20018354,-0.09572778,-0.01234459, 0.09102707],
         [ 0.07692331, 0.16519296,-0.13493372,-0.00481602],
         [ 0.11505011, 0.00630549, 0.16963176,-0.1265313 ],
         [ 0.02964999,-0.16150073,-0.03567221, 0.19607036],
         [-0.25053656,-0.17077265, 0.18452801, 0.04208443],
         [ 0.20707684, 0.30037447, 0.29423379, 0.2844169 ],
         [ 0.03275292,-0.25770278, 0.15206177, 0.04066527]],0.0),
       ([[ 0.15549408,-0.02821391,-0.21777784, 0.31366812],
         [ 0.24646228, 0.19210799,-0.05598768,-0.18141553],
         [-0.1300077 , 0.07774861,-0.15924152, 0.18953487],
         [-0.30428559,-0.01242687, 0.14800427, 0.11609853],
         [-0.04978502, 0.12219252,-0.08399155,-0.16921891],
         [ 0.18028181, 0.14338093, 0.13057504,-0.29615267],
         [-0.16479341,-0.08155035, 0.05178466,-0.26057883],
         [-0.16941652, 0.19765246, 0.15984705, 0.19275784],
         [-0.13000542, 0.17747904,-0.17406042,-0.13291107],
         [ 0.22071552, 0.15442383,-0.10609241, 0.042586  ]],0.0),
       ([[ 0.12626878,-0.25808267,-0.28220025, 0.22945244],
         [ 0.08220049, 0.23378918,-0.26076719, 0.2314179 ],
         [-0.29014052, 0.28977474,-0.17242757, 0.15929158],
         [-0.06277205,-0.06130048,-0.15603502, 0.00432502],
         [-0.19950167,-0.28012466,-0.08416305, 0.13903731],
         [-0.01633346, 0.26728853,-0.20863298, 0.13811676],
         [ 0.23388056,-0.2344451 , 0.29981922, 0.16078008],
         [ 0.26352996,-0.2923824 ,-0.13544108, 0.20422487],
         [-0.14247096,-0.28351467,-0.22266022,-0.30579689],
         [ 0.17861596,-0.09542387, 0.15791013,-0.28147219]],0.0),
       ([[-0.26677087, 0.18299502,-0.08194217,-0.24604896],
         [-0.05994665, 0.09568975,-0.04029488,-0.18817124],
         [-0.03548735, 0.1531479 ,-0.25550249, 0.00741318],
         [ 0.21732212, 0.31214441, 0.08269747,-0.13567858],
         [-0.23652461,-0.30778088,-0.10235937,-0.00991679],
         [ 0.02921378,-0.29343446,-0.27750602,-0.30157962],
         [ 0.00467349,-0.1476208 ,-0.03459167, 0.2995368 ],
         [ 0.18833028,-0.13653101, 0.03528241,-0.19126215],
         [ 0.18379212,-0.26081224, 0.21372283,-0.16363993],
         [-0.04157616,-0.04562434,-0.25164319, 0.25855651]],0.0),
       ([[-0.22198474, 0.25776865,-0.30961634,-0.20647498],
         [ 0.21449012,-0.1200055 , 0.22071792, 0.26015676],
         [ 0.25291967, 0.23225578, 0.21361957,-0.15580994],
         [ 0.21693739, 0.07439454, 0.3062204 , 0.06991173],
         [ 0.09594247, 0.09769235, 0.0330947 ,-0.19876174],
         [ 0.30419617,-0.25555211, 0.15701523, 0.01557005],
         [-0.13233517,-0.17190726, 0.00236485, 0.2731937 ],
         [ 0.04688379, 0.30974169,-0.04441544,-0.05290127],
         [ 0.08087671, 0.25764235,-0.27232097, 0.03187775],
         [-0.21844645, 0.13822999,-0.23058892,-0.31335233]],0.0),
       ([[ 0.02372731,-0.30573356, 0.02246136,-0.28732956],
         [-0.21884266, 0.21693885, 0.18842711,-0.12421786],
         [-0.06419842,-0.03206922,-0.30324018, 0.07434775],
         [ 0.19261741,-0.16812438,-0.06316435,-0.0042285 ],
         [ 0.00118972,-0.1053113 ,-0.24955413, 0.0890547 ],
         [-0.22043028,-0.19278721,-0.27993022, 0.26230748],
         [ 0.20913413,-0.21566463, 0.23255812, 0.00461579],
         [-0.23508285,-0.24468582,-0.17531808, 0.24655455],
         [-0.0310396 , 0.12472635,-0.0693866 , 0.07755408],
         [ 0.13689381, 0.00054343, 0.16448981, 0.21035708]],0.0),
       ([[ 0.03791167, 0.02773479, 0.05563838,-0.30031882],
         [ 0.30933518,-0.27146622, 0.20087747,-0.22955874],
         [-0.21944965,-0.08902337,-0.15592404,-0.17786902],
         [ 0.04008723,-0.01260965,-0.25737976, 0.00879528],
         [-0.19136979,-0.01247813,-0.14102477,-0.30959726],
         [ 0.24525437,-0.07975361,-0.05298902, 0.23880414],
         [ 0.16492601, 0.03042894, 0.25446277, 0.26607488],
         [-0.02185716, 0.04151866,-0.29816608,-0.22747489],
         [-0.0632467 , 0.20687477,-0.02821297, 0.12243277],
         [ 0.08726643, 0.09858908, 0.17033229,-0.31608404]],0.0),
       ([[-0.12605127, 0.12362702,-0.25874551, 0.01495601],
         [ 0.24956381, 0.29327334, 0.13008863, 0.16698653],
         [-0.09827862,-0.15694363, 0.10575233,-0.22179898],
         [-0.0183939 ,-0.30212911, 0.16425569,-0.18615881],
         [ 0.21540833, 0.17383314,-0.20481184, 0.24321633],
         [-0.25736895,-0.18835257,-0.08069786,-0.20639825],
         [-0.09788278, 0.04284158,-0.21696261, 0.17439356],
         [-0.29493831,-0.30929699,-0.11376398, 0.01690371],
         [ 0.08204628, 0.24129053, 0.08926425,-0.30187867],
         [ 0.12344918, 0.24327516, 0.15899559,-0.14984265]],0.0),
       ([[ 0.00479053,-0.23768247, 0.06876417, 0.12226175],
         [-0.26703143,-0.06066419, 0.00852986, 0.19420577],
         [-0.25363598,-0.01737246, 0.04259072, 0.02275809],
         [-0.19968968, 0.05488643, 0.18082602, 0.31500631],
         [-0.28287864,-0.28923529, 0.01829978, 0.06538291],
         [-0.26420614, 0.13269908,-0.2427754 ,-0.19144272],
         [ 0.17663563,-0.02790462,-0.1208941 ,-0.08368919],
         [ 0.16653039,-0.20210276,-0.22911153, 0.00054447],
         [-0.30680258,-0.09657957,-0.10262399, 0.00292908],
         [-0.00481986,-0.09468136,-0.18822509,-0.21507023]],0.0),
       ([[ 0.03859024,-0.03404153, 0.22471083, 0.26005346],
         [ 0.23816049, 0.29436329,-0.18827486, 0.14317266],
         [ 0.04510547, 0.0530803 ,-0.02737148, 0.05193146],
         [-0.27337999,-0.31246433,-0.02023115, 0.18355614],
         [ 0.26749634, 0.13929289,-0.23658199, 0.13856624],
         [ 0.07304499,-0.07164557, 0.1803852 ,-0.25213215],
         [-0.06138748, 0.15330512,-0.30836393, 0.2708065 ],
         [-0.02515688,-0.27219304, 0.25926305,-0.23824315],
         [ 0.24968185,-0.18653642, 0.2732137 ,-0.00130973],
         [-0.08854121,-0.21448389, 0.14317274, 0.21204644]],0.0),
       ([[-0.11455104, 0.30235508,-0.01725896, 0.04183339],
         [-0.2234564 ,-0.25314386,-0.26150953, 0.16001999],
         [ 0.15323721,-0.11668402, 0.21839602, 0.02184922],
         [ 0.21033596,-0.19856954,-0.14169975, 0.05253024],
         [ 0.27192518,-0.27999097, 0.20130892,-0.21211106],
         [-0.03813321,-0.08441968,-0.27462561, 0.18202632],
         [ 0.0078349 ,-0.20438528, 0.01317482, 0.05755919],
         [-0.03507686,-0.22578156, 0.29172904,-0.05200696],
         [-0.23399265, 0.09549641, 0.05559626,-0.15511762],
         [-0.13443928, 0.1930753 ,-0.28067894,-0.14028389]],0.0),
       ([[ 0.27620054, 0.21402711, 0.25815181, 0.0598411 ],
         [ 0.17297742,-0.09859982,-0.07344879,-0.07719674],
         [-0.04682438, 0.13085369,-0.1618007 ,-0.20378896],
         [-0.17932299, 0.14191695, 0.08315969, 0.18628023],
         [ 0.18070425,-0.22453606,-0.2608954 ,-0.15384043],
         [ 0.08861803, 0.04408714, 0.24624226, 0.26445614],
         [-0.21985107,-0.24097504,-0.30986928,-0.13924173],
         [-0.238485  ,-0.0267933 ,-0.1970722 ,-0.31141385],
         [ 0.25066615, 0.31468011, 0.19353396,-0.29714789],
         [ 0.02234053, 0.06957043, 0.22225983,-0.05694756]],0.0),
       ([[-0.15090577, 0.16114397, 0.18409262, 0.18169796],
         [ 0.28320212,-0.06311385, 0.10006483, 0.01621376],
         [-0.05177097,-0.0584798 ,-0.08141669, 0.2174979 ],
         [-0.01845121, 0.05369522,-0.21141849, 0.02982549],
         [ 0.02455099, 0.00482561, 0.0904923 , 0.02826048],
         [-0.11494598, 0.18226821, 0.04074616,-0.05339262],
         [-0.1088262 , 0.26604124, 0.27225267,-0.30946571],
         [-0.25735736,-0.23142216, 0.08053501,-0.15071939],
         [ 0.05220475, 0.03472307,-0.1895212 , 0.09934619],
         [ 0.28030865,-0.09058807,-0.29611488,-0.01674297]],0.0),
       ([[-0.27356384,-0.0004719 , 0.12152237,-0.26808676],
         [-0.17087049,-0.00166471, 0.02706323,-0.00320345],
         [ 0.19054808,-0.12145723,-0.24411533, 0.03812508],
         [ 0.17019938,-0.04099139,-0.2633963 , 0.06235624],
         [ 0.21887018,-0.2003267 , 0.26118465,-0.25067232],
         [-0.23601734,-0.31365088, 0.17998991, 0.03824993],
         [ 0.10152185, 0.01380161, 0.18704629,-0.15707403],
         [ 0.0192782 ,-0.28061783,-0.22937106,-0.22642663],
         [ 0.10357595,-0.24802406, 0.16256293,-0.02893172],
         [ 0.12277711,-0.09061356, 0.16950929, 0.20069569]],0.0)]

ff1 :: [FilterF]
ff1 = [
  [ 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
  , 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
  [ 0.01066859,-0.00732406, 0.00252391,-0.01163805,-0.00517526,-0.01144248
  ,-0.01006831,-0.00497709,-0.01099333 ,0.00207945, 0.00561041, 0.00626921
  , 0.00495249,-0.00966603,-0.00532601,-0.00594638,-0.0108443 ,-0.01244558
  ,-0.00576639,-0.01165473],
  [-0.00080344 ,0.00102066,-0.00738061,-0.00678004, 0.00383328,-0.01200945
  ,-0.0050146 ,-0.00048961, 0.00053184, 0.01147495, 0.00651699, 0.00758566
  ,-0.00426098,-0.00308052, 0.00467642, 0.00518132,-0.00972884,-0.00580618
  ,-0.01173668,-0.00247515],
  [-0.01234694, 0.00422179, 0.00922248,-0.00310442,-0.00547224,-0.00996623
  , 0.00764289,-0.00039315, 0.00941688,-0.00869446,-0.00700709, 0.00583178
  ,-0.01086199, 0.00788778, 0.00899433,-0.01170974,-0.01082992,-0.00067387
  ,-0.00735956,-0.00686391],
  [-0.00164054,-0.00667725, 0.00821022, 0.00978605, 0.00785767, 0.0115325
  , 0.00409618,-0.01135108, 0.00938884, 0.01107375, 0.01172765, 0.00623253
  , 0.01081841,-0.01028971, 0.00835867, 0.00578451, 0.01153615,-0.00963332
  ,-0.00591298,-0.00756431],
  [-0.00886026,-0.00576119,-0.01040237,-0.00760641, 0.01118798, 0.01174343
  ,-0.01240258,-0.00789268, 0.01069915, 0.00466251, 0.00786986, 0.01225197
  ,-0.01048275, 0.00183779, 0.00916296,-0.00633987, 0.00233338, 0.01189655
  ,-0.00765472,-0.00517659],
  [-8.95062482e-05 , 3.84380076e-03 , 2.66531514e-03 , 5.19685704e-03
  ,-4.64497798e-03 ,-1.35366224e-03 ,-4.97275472e-03 ,-1.56790392e-03
  ,-1.12306342e-02 , 1.13377847e-02 ,-8.02766457e-03 ,-1.05811820e-02
  , 1.09196734e-02 , 8.39562151e-03 , 1.18154652e-02 , 8.07937093e-03
  ,-1.16724095e-02 , 1.73742671e-03 ,-8.81212330e-03 , 2.00135084e-03],
  [-0.01041273,-0.00111782, 0.00897893,-0.00436059,-0.00406013, 0.01045341
  , 0.00343008,-0.00835123, 0.00935308,-0.01011083, 0.00225307, 0.00331874
  ,-0.00418363,-0.0084705 , 0.00351936,-0.0028065 , 0.0079132 ,-0.00676701
  , 0.00412791, 0.00189019],
  [-0.0105157 ,-0.01144195, 0.00892556,-0.00881145,-0.00992306,-0.00984128
  ,-0.00516871, 0.00075292, 0.00915814, 0.00921959,-0.00358742,-0.01004681
  ,-0.00154432, 0.01118716,-0.01146012,-0.00472881, 0.00705717, 0.00738775
  , 0.00881452, 0.00109728],
  [ 0.00226571,-0.0090223 ,-0.01163191,-0.00861851, 0.00642299,-0.00328044
  ,-0.00542969,-0.00645503, 0.0092838 ,-0.00332264, 0.00970089, 0.01124697
  , 0.00931897,-0.00319887, 0.00364642,-0.00168274, 0.00576501, 0.00497362
  ,-0.00174329,-0.00240321],
  [ 0.00252765, 0.00372214,-0.00964801, 0.00513378,-0.00350677,-0.00192684
  ,-0.00466145, 0.00170765,-0.01166087, 0.011921  , 0.01176473,-0.00544318
  , 0.00394552, 0.00692715, 0.01113738, 0.0042027 ,-0.001237  , 0.00850632
  , 0.00806301, 0.01075456],
  [ 0.00505145, 0.00751564, 0.01147363,-0.0028529 , 0.00581275,-0.00928688
  ,-0.00736665, 0.00957956, 0.00489559, 0.00527258,-0.00325522, 0.00195685
  ,-0.01040478, 0.00235457,-0.00807567,-0.00197832,-0.00650315,-0.00931179
  , 0.00989005, 0.01025191],
  [ 0.00322198,-0.00856098, 0.00576474, 0.01222741, 0.00699369, 0.01198837
  , 0.00762371,-0.00200669,-0.00087365, 0.00640988, 0.00775375,-0.00896642
  ,-0.01047096,-0.00356123, 0.0117198 , 0.0047932 ,-0.01193419,-0.00717945
  , 0.00565617, 0.00433045],
  [-0.01154009,-0.00353282, 0.00627853, 0.00914179, 0.01135541, 0.00948912
  , 0.00914906, 0.00530143, 0.00409428,-0.00288013,-0.0080174 , 0.0061133
  , 0.0066756 ,-0.00858631,-0.01244245, 0.00054219, 0.00646428,-0.00147078
  , 0.00971396, 0.00073288],
  [ 9.62478843e-03 ,-1.18308457e-03 , 4.59782139e-03 ,-1.09700094e-02
  , 6.01343116e-03 , 4.31491864e-03 ,-1.22401050e-02 , 4.33054751e-03
  , 7.24811990e-03 , 8.26936547e-03 , 1.23595508e-02 , 1.67638710e-03
  , 9.03904017e-03 ,-1.02207202e-02 ,-8.27717055e-03 ,-4.23481756e-03
  , 1.13682138e-02 ,-9.90754241e-03 ,-6.91719615e-05 , 1.22271905e-02],
  [ 0.00288834, 0.00142354,-0.00656423,-0.0007906 ,-0.00077881, 0.00157819
  ,-0.0022085 ,-0.00806158,-0.00869178, 0.00624086,-0.00027861, 0.00573193
  ,-0.00187579, 0.00564066, 0.00220019,-0.00907322, 0.00161867,-0.00513814
  , 0.00334987, 0.00358088],
  [ 0.00131255,-0.00274947,-0.00582195, 0.00755416, 0.0099909 ,-0.0099603
  ,-0.0011555 ,-0.00264943, 0.00429109, 0.00134484, 0.00857272,-0.01235088
  , 0.0036477 ,-0.00962859, 0.00640938,-0.00679897,-0.00703655,-0.00221649
  ,-0.01078762,-0.00078119],
  [-0.00363904,-0.01043245,-0.00910429, 0.01031767, 0.00929819, 0.00709161
  , 0.00537091, 0.00425848, 0.00335479,-0.00273316, 0.00666634,-0.00695726
  , 0.00516894,-0.00488614, 0.00709306, 0.00264995, 0.00929371,-0.01035156
  , 0.0011413 , 0.01038863],
  [ 0.00281654, 0.00148329,-0.00427738, 0.00831511,-0.00637084,-0.00529944
  , 0.0025652 ,-0.0027675 ,-0.00219223, 0.00082132, 0.00939818, 0.00660784
  , 0.00879295, 0.00849922, 0.00117726, 0.00111854,-0.01227962, 0.00299258
  , 0.01030925,-0.0071851 ],
  [-0.00444623,-0.00595885, 0.00983405, 0.00806127,-0.00595873,-0.0073279
  , 0.00065924,-0.01206427,-0.00206536,-0.01027443, 0.012182  , 0.01123721
  , 0.00630392, 0.00350661,-0.00729056,-0.00830335,-0.008907  ,-0.00117884
  ,-0.01238822,-0.00463047],
  [ 0.00276137, 0.01234733, 0.00053951,-0.00993308,-0.01119019,-0.00428539
  , 0.00149404, 0.00844385,-0.01109716, 0.009873  , 0.00989499,-0.00119354
  , 0.01012319,-0.00867025, 0.00591153,-0.0006324 , 0.01185267, 0.00064205
  , 0.00895278, 0.00797076],
  [ 8.46392068e-03 ,-7.79041756e-03 , 1.06217236e-02 , 5.15797895e-03
  ,-1.10302970e-02 , 9.59077982e-03 ,-4.05677078e-03 , 1.23950282e-02
  ,-9.48145438e-04 , 7.77296737e-03 , 5.08556735e-03 , 1.15169887e-02
  , 5.37327158e-03 ,-1.19653024e-04 , 1.18686152e-02 , 5.67367498e-03
  ,-7.76135954e-05 ,-4.23448640e-03 ,-3.26929525e-03 ,-1.05783885e-02],
  [ 6.69144565e-03 , 1.06820265e-02 ,-1.93337863e-03 , 5.22328038e-04
  ,-1.22761473e-02 ,-1.74181513e-03 ,-8.67346086e-03 ,-4.65271136e-04
  , 5.56034148e-05 ,-2.21444601e-04 , 7.32540339e-03 ,-1.12624719e-02
  , 5.40851690e-03 , 1.28164497e-03 ,-3.88831000e-03 , 1.10353601e-02
  , 6.33282554e-03 , 7.53019085e-03 , 3.17577786e-03 , 1.20712027e-03],
  [ 0.00544021, 0.00072116, 0.00048279,-0.00652573, 0.00573862, 0.01240188
  , 0.00429685, 0.00178347,-0.00357252,-0.01089316, 0.00519549,-0.00663018
  , 0.00713186,-0.00350113,-0.01071695, 0.00713012,-0.00278745,-0.00358081
  , 0.00040233, 0.0083142 ],
  [-0.00495888, 0.00103415,-0.00442433,-0.00937942,-0.0082654 ,-0.01048515
  ,-0.00092459, 0.00896584,-0.00989112, 0.00882469, 0.00242808, 0.0103896
  ,-0.00805709, 0.01205256, 0.0038596 ,-0.00894956,-0.01033238,-0.00238587
  ,-0.0018353 , 0.00819368],
  [ 0.00376443,-0.01202793, 0.01011705, 0.00076125,-0.01050199, 0.00311676
  ,-0.00085068,-0.00457551,-0.00558411, 0.01120614, 0.00541951,-0.00604338
  ,-0.00377723,-0.00073079,-0.01154582,-0.00502359,-0.00320586, 0.00304174
  ,-0.01182568, 0.01048871],
  [-0.00652835,-0.00906475, 0.01093555, 0.00640548, 0.0113222 , 0.00899148
  , 0.00909307,-0.00236257, 0.00491269,-0.00941265, 0.01206549,-0.00516477
  ,-0.0110909 , 0.01086352, 0.00371949,-0.00386044, 0.00108276, 0.00943728
  , 0.00926487, 0.01191473],
  [ 0.01203412,-0.00160259,-0.01039249,-0.00307191, 0.01113244, 0.00052112
  , 0.00714226, 0.00903917,-0.00176562, 0.00463601,-0.0058809 ,-0.00762491
  , 0.00801294, 0.0005071 ,-0.00266263,-0.01147294,-0.00947638, 0.00493746
  ,-0.00434057,-0.00485281],
  [ 0.00936561, 0.00911779, 0.00586914,-0.0055716 ,-0.00356154,-0.00556621
  , 0.00352484,-0.00945462,-0.00319551,-0.0012367 ,-0.00273598, 0.00789209
  , 0.0118411 , 0.01051535,-0.01002551,-0.00953866, 0.01201148,-0.00888954
  , 0.01008033, 0.00614788],
  [-0.00378852,-0.01226641, 0.00403118, 0.00690344, 0.01066544,-0.00976459
  ,-0.01075284, 0.00969608, 0.00514754,-0.00710446, 0.00556285, 0.01197618
  ,-0.01099265, 0.00537295,-0.00802576, 0.00905026, 0.00323194,-0.00462366
  ,-0.0066517 , 0.0045738 ],
  [ 0.00335928, 0.00234975, 0.00383195,-0.00852078, 0.00973228,-0.0017548
  , 0.01059189,-0.00101989, 0.00323971,-0.00945476,-0.00673255,-0.01198771
  ,-0.01042687,-0.00844839,-0.00381801,-0.00291249, 0.00541536,-0.00048124
  , 0.00931966, 0.00020103],
  [ 0.01207587,-0.00184462, 0.00055029, 0.01116861, 0.00037045,-0.00311918
  , 0.0014292 ,-0.00111827, 0.01111491, 0.0061765 , 0.0023189 , 0.01086487
  , 0.00894866, 0.00311131, 0.00667968, 0.00746065, 0.00741078,-0.01023526
  ,-0.01185849,-0.00364867],
  [-0.00542996,-0.01110709,-0.01080439,-0.00995801, 0.00291741,-0.00744836
  , 0.00949581, 0.01242521, 0.00709297, 0.00562986,-0.00531967, 0.00542752
  , 0.00376833,-0.0091284 ,-0.01087973,-0.00487021, 0.00485225, 0.00221893
  , 0.00543267, 0.00934875],
  [ 0.00763313,-0.01002816,-0.00482714,-0.00478156, 0.00350477,-0.00414792
  ,-0.00711926,-0.01203998, 0.00907464,-0.00971987,-0.00619828, 0.01231174
  , 0.0081969 ,-0.00637361,-0.01116121,-0.00716985, 0.00356197, 0.00665433
  , 0.00627556,-0.01110167],
  [ 0.00296623,-0.003842  ,-0.0079539 ,-0.00202336,-0.0055312 ,-0.0112696
  , 0.0083446 , 0.00784507,-0.00382613, 0.00629391,-0.01202155, 0.00546238
  ,-0.00897627, 0.00388857,-0.00161949, 0.00670043,-0.00726473,-0.0040551
  , 0.00016447,-0.00522   ],
  [ 0.00429089,-0.01186585,-0.00440458,-0.01017757,-0.00789433, 0.00823425
  ,-0.01059612, 0.00053919, 0.01210166,-0.00189402, 0.00339565,-0.00161079
  , 0.00132072, 0.00712222,-0.00843742, 0.00207101, 0.00176048,-0.00862461
  ,-0.00627789,-0.00754868],
  [-0.00533027, 0.00194299, 0.00189309, 0.00680252,-0.00225482, 0.00417262
  , 0.00920791,-0.00426218,-0.0111613 , 0.0073511 , 0.00358456,-0.00134936
  ,-0.01216447,-0.00287755,-0.01120505, 0.00076328,-0.00726214,-0.00565584
  ,-0.00720149,-0.00433215],
  [-0.01249892,-0.00582993, 0.01125316,-0.00205335, 0.00112274, 0.00277004
  ,-0.01124297,-0.00945938, 0.00015742, 0.00243568,-0.00564697,-0.01177121
  , 0.00951682, 0.0102917 ,-0.0102379 , 0.00248273, 0.00470412,-0.01105967
  ,-0.01038476,-0.00416708],
  [ 0.00226436, 0.01071674,-0.00986439,-0.0027656 , 0.00636616, 0.00724571
  ,-0.01224635, 0.00148881,-0.00047424,-0.00490778, 0.01226964, 0.00288426
  , 0.00839314,-0.00100588, 0.00331049, 0.00860655, 0.00427553,-0.00882808
  ,-0.00799431, 0.01027652],
  [ 0.00361652, 0.01031178, 0.00133659, 0.00670242,-0.01140393,-0.00430469
  , 0.01054206, 0.00204753, 0.0055278 , 0.00292928, 0.00648048, 0.01053789
  , 0.00599224,-0.00730118, 0.01024542, 0.00102098, 0.00382307,-0.00047306
  , 0.00158571, 0.00818619],
  [ 0.002208  ,-0.00074919, 0.00210228,-0.00769716, 0.0115142 ,-0.00229353
  , 0.00801872,-0.00400358,-0.01136765, 0.0085178 ,-0.00922463,-0.01206789
  ,-0.00807628,-0.00198466,-0.00874544,-0.00664754,-0.00962095,-0.00517162
  , 0.00660944,-0.01064153],
  [ 0.00320883,-0.00847903, 0.01097676,-0.01200392,-0.00388097,-0.00960573
  , 0.00704474, 0.00644757,-0.00056461,-0.01170432, 0.00588037, 0.00323874
  ,-0.00849887, 0.00304199, 0.00987759, 0.00577641,-0.01124088,-0.00863869
  , 0.00166354,-0.0114758 ],
  [-0.00206452,-0.01158154, 0.01087855, 0.01230348, 0.00125331, 0.00792299
  ,-0.0032712 ,-0.00556204,-0.0066489 , 0.01037679, 0.00772508, 0.00286007
  , 0.00759801, 0.00696668,-0.0003919 ,-0.00931529,-0.01186137, 0.01066823
  ,-0.0095517 , 0.00348103],
  [ 0.00013424,-0.00238304,-0.00884833,-0.00796038, 0.00250146, 0.00860145
  , 0.00086539,-0.00455901, 0.00634047,-0.00801671, 0.00158748, 0.00369393
  , 0.00679768, 0.0086455 , 0.00826234,-0.00179187,-0.00835937, 0.00111937
  , 0.00960927, 0.00529514],
  [ 0.00140926, 0.00363086,-0.01168287,-0.00757898, 0.01002137,-0.00787441
  ,-0.00936265, 0.01204393,-0.00655697, 0.01096797,-0.00125145, 0.00295816
  , 0.00368476,-0.00270485, 0.01199798,-0.00139161, 0.00769199, 0.01174091
  , 0.00208815, 0.00370353],
  [ 0.0027834 , 0.00357384, 0.00826255,-0.01097698,-0.00544016,-0.00598417
  , 0.01239699, 0.00408046, 0.00794754,-0.0057143 ,-0.00520204, 0.00249492
  , 0.00244948,-0.00392808,-0.00581416,-0.00990428,-0.0083222 ,-0.00660226
  , 0.0112707 , 0.00205546],
  [-2.72668540e-03 ,-3.04619880e-05 ,-2.07841948e-03 ,-8.69518718e-03
  , 5.29521006e-03 ,-9.87452257e-03 ,-1.08234007e-02 , 1.10915649e-02
  , 7.32519973e-03 , 9.15726036e-03 ,-2.65033594e-03 ,-1.08417200e-03
  , 4.21936459e-03 ,-6.02962666e-04 , 7.00445853e-03 , 8.65067840e-03
  , 7.89468121e-03 ,-7.55053598e-03 ,-9.76909436e-03 , 5.35649594e-03],
  [-0.00179035, 0.01179558, 0.00904097,-0.00274443, 0.00962619, 0.00436441
  ,-0.00985929, 0.00193207, 0.00385781, 0.01165262,-0.00758334,-0.00224772
  ,-0.00057861, 0.00020277, 0.003209  ,-0.0027973 ,-0.0113159 , 0.00861142
  , 0.00414189,-0.00950504],
  [ 0.00209376, 0.005146  ,-0.00303589, 0.00791125, 0.00346253, 0.01078335
  ,-0.00123744,-0.00300114,-0.00814488,-0.0039286 ,-0.00080868,-0.00491568
  , 0.01036621,-0.00147612, 0.00464938,-0.0027311 , 0.00669097,-0.0071758
  , 0.00467964, 0.00448062],
  [ 0.00938512, 0.001243  ,-0.00941365,-0.00383839,-0.00286666,-0.01095067
  , 0.0079428 , 0.01202932,-0.00369249,-0.01048368, 0.00764393,-0.00862739
  ,-0.00299994, 0.01206674,-0.00573573,-0.00683315, 0.00370023, 0.00108963
  ,-0.00090541, 0.00700346],
  [ 0.00354747,-0.00361291,-0.00297248,-0.00767082,-0.00407765, 0.00317918
  , 0.0009354 , 0.01050761, 0.00525623, 0.00090573, 0.00358177,-0.0028821
  ,-0.00907781, 0.01135075,-0.00556214, 0.00571194,-0.01014144,-0.00674322
  , 0.00997762, 0.00489356],
  [ 0.00011393,-0.00818637, 0.00790257, 0.00097393, 0.00033252, 0.0021809
  ,-0.00249161,-0.01150878,-0.00987804, 0.00261858,-0.01063114,-0.00173847
  , 0.00856298, 0.00815883,-0.00264958,-0.00833218,-0.0011173 ,-0.01131523
  ,-0.00287254, 0.00153397],
  [ 0.00826241, 0.00314836,-0.00999148, 0.0025756 ,-0.00254659,-0.00411739
  , 0.00918136, 0.01244321, 0.00358923, 0.0056338 , 0.00031878, 0.00820766
  , 0.00293135, 0.01174047,-0.00501334, 0.00864324, 0.0011811 ,-0.0114583
  ,-0.01024605, 0.0121972 ],
  [-0.00672257,-0.00921353,-0.00571511,-0.01025691,-0.01121458,-0.00170732
  , 0.00519282, 0.01065435, 0.00997637,-0.00435491,-0.00795616, 0.00697481
  , 0.00348538, 0.00129915, 0.00166591, 0.00652075, 0.00314469,-0.00536196
  ,-0.00653247,-0.00194938],
  [-1.03523599e-02 , 5.97105496e-03 ,-5.08734262e-03 ,-3.69286060e-03
  , 1.32695964e-03 ,-6.55843437e-03 ,-8.41936850e-03 ,-3.16304254e-03
  , 9.24870630e-03 ,-1.18969889e-05 ,-4.28425954e-03 , 2.12929439e-03
  ,-1.13912339e-02 , 1.11039647e-02 , 9.88130315e-03 , 9.53516192e-03
  ,-3.62213202e-03 , 1.43527142e-03 , 3.73018101e-03 , 7.03908336e-03],
  [-0.01236657, 0.00530999,-0.0116527 , 0.00820043, 0.00527677,-0.00016446
  ,-0.01224506, 0.00192237,-0.00894374,-0.00967588,-0.00767405, 0.00615673
  ,-0.00706696,-0.00958262, 0.00510878, 0.00859685,-0.00043686,-0.01025731
  , 0.00799814,-0.00501565],
  [-0.00746371, 0.00215906,-0.00043919, 0.00901862, 0.00741798, 0.00806875
  , 0.00535268, 0.00645328, 0.00194922, 0.01202378, 0.0015911 ,-0.00967699
  ,-0.00465716, 0.0020571 , 0.00485642,-0.000762  ,-0.00086573,-0.00858158
  , 0.01041429, 0.00554139],
  [ 0.00148777,-0.00791657, 0.00517407, 0.00211549,-0.00089054,-0.00415529
  , 0.00148619,-0.0101497 , 0.00726114, 0.00027298,-0.00774654, 0.00366346
  , 0.01082807,-0.00754704, 0.01018217,-0.00233782, 0.00033159, 0.00263305
  , 0.00744067,-0.00748181],
  [-0.00530756, 0.0082105 , 0.01012584, 0.00578604,-0.00579202,-0.00070064
  ,-0.01052903,-0.01227684, 0.00145826, 0.01139462,-0.01163602,-0.01087151
  , 0.00197327,-0.00577476,-0.00051975,-0.00465957, 0.00930706,-0.0023245
  ,-0.00978318,-0.00057441],
  [ 0.0120561 ,-0.01082798, 0.00880624, 0.01213872,-0.00706125,-0.01137924
  , 0.01200272,-0.00108731,-0.01110328, 0.00961684,-0.0083173 , 0.00710454
  ,-0.00910621,-0.00637475, 0.01037462,-0.00461774,-0.00295659,-0.00868007
  ,-0.00118485, 0.01115873],
  [ 0.00334316, 0.00203303,-0.00967899, 0.00360178,-0.00198238, 0.01004799
  , 0.00715754, 0.01062888,-0.00406039, 0.00550234, 0.00809305,-0.00081077
  ,-0.00403245, 0.01050833, 0.0109398 , 0.00428197,-0.00902488, 0.01072406
  , 0.00717519,-0.00100316],
  [-1.22546920e-02 , 6.75952665e-03 ,-4.23471541e-03 , 1.07877352e-03
  , 5.85293044e-03 , 9.57105931e-03 ,-1.09394601e-02 ,-6.01607234e-03
  , 8.04489849e-04 ,-1.77657018e-04 , 1.08114452e-02 ,-1.11144571e-02
  , 1.14143162e-02 ,-3.99754590e-03 , 1.60453176e-03 , 4.75551096e-03
  ,-7.40113352e-05 ,-1.20711955e-02 , 8.49964831e-03 , 6.55495600e-03],
  [-0.00406278,-0.00045401,-0.00306099,-0.00064802, 0.00519738, 0.0035539
  ,-0.00206664,-0.00838678, 0.00136386,-0.00420693, 0.00863283,-0.00861135
  ,-0.00825615,-0.01110408, 0.01167495, 0.001208  ,-0.00450007,-0.00914162
  ,-0.00194327, 0.0115132 ],
  [-0.00303781, 0.01108009, 0.00795451, 0.00139012, 0.01102056,-0.00256599
  , 0.01067795,-0.00056947,-0.01119662,-0.01194921,-0.0072646 ,-0.01178812
  ,-0.00153214,-0.00479203,-0.00877115, 0.00747526, 0.00393389, 0.00325965
  , 0.00830543, 0.00940472],
  [ 0.00266878,-0.00529145, 0.00412996, 0.0080954 , 0.01153203,-0.01155425
  , 0.00605895, 0.00360714, 0.00430706, 0.00861343, 0.00422972, 0.00657355
  , 0.00084631, 0.00353301, 0.00111121, 0.00530294,-0.01105768,-0.001239
  , 0.0068234 , 0.00830379],
  [-0.01082984, 0.01109784,-0.0089807 , 0.00390231,-0.00964654, 0.00616796
  , 0.00130396,-0.01111575, 0.00881722, 0.00349989,-0.00178043,-0.01026392
  ,-0.01154074,-0.00560485,-0.00323253, 0.00967721,-0.01135846, 0.00101323
  ,-0.0043407 ,-0.00620226],
  [ 3.43828345e-03 ,-7.30941986e-03 ,-1.11230350e-02 , 5.82274076e-03
  ,-8.28103072e-03 ,-8.92918074e-03 ,-2.12909992e-03 ,-5.44803965e-03
  , 1.61940084e-03 , 1.94805111e-04 ,-9.70111127e-03 ,-7.41207689e-04
  ,-8.89090843e-03 ,-4.66256681e-03 , 3.14404349e-03 , 9.79219412e-03
  , 1.99991394e-03 , 5.67633585e-03 ,-9.29591384e-05 , 2.27469659e-03],
  [-0.00145541, 0.00349509,-0.01062648,-0.00116451,-0.00564535, 0.00958521
  ,-0.00776804,-0.00381133, 0.00790283, 0.00908506,-0.00391282,-0.01066467
  ,-0.00231371,-0.00455363,-0.00892973, 0.00932652,-0.0064667 , 0.01073669
  , 0.00570167,-0.00329525],
  [-4.00771249e-03 ,-1.08727125e-02 ,-2.02900250e-03 , 1.39850943e-03
  ,-2.86391299e-03 ,-8.00004684e-03 ,-4.77394057e-05 ,-1.42634178e-03
  , 9.99311106e-03 ,-3.79250514e-03 ,-1.24716277e-02 ,-7.51943716e-03
  , 1.19361249e-04 , 6.44561632e-03 , 1.03384441e-02 ,-6.61578013e-03
  , 3.72136248e-03 , 2.20028736e-03 , 4.75189729e-03 , 4.90850901e-03],
  [-0.00208483, 0.00631022,-0.00139168,-0.01001081, 0.009591  , 0.00074903
  , 0.00728791, 0.01097036,-0.00632229,-0.00432326, 0.00778356,-0.00226321
  ,-0.00552436, 0.00798108, 0.00321203, 0.00217065, 0.00225468, 0.00151688
  ,-0.00538489, 0.00574373],
  [-9.28805236e-03 ,-2.30969784e-03 ,-4.27667759e-05 ,-2.38770509e-03
  ,-9.94480304e-03 ,-1.03943721e-02 , 1.19328168e-02 , 1.18849092e-02
  , 1.21502040e-04 , 7.87470504e-03 , 6.47686263e-03 ,-8.53464587e-04
  ,-1.46435903e-03 , 9.93296970e-03 ,-4.50290817e-03 , 5.22748349e-03
  ,-1.22758806e-02 , 5.21199040e-03 , 3.75700472e-03 ,-1.05835959e-02],
  [ 0.00020407, 0.01098674, 0.00313563, 0.00638352,-0.00509281,-0.00517045
  ,-0.01017023,-0.0032837 , 0.00586701,-0.01166983,-0.00326257,-0.00463854
  , 0.00450383,-0.01079017, 0.00487205,-0.0063211 , 0.00291824, 0.00369205
  , 0.00062676, 0.00694075],
  [-0.01221537,-0.0093713 , 0.00857291, 0.00403789, 0.00293199, 0.01219605
  , 0.00415561,-0.00711345, 0.01046225,-0.00595286, 0.00726636, 0.00023476
  , 0.00418391, 0.00099265,-0.01034387, 0.00169653, 0.00798995,-0.01239525
  , 0.00971512,-0.00832666],
  [ 0.01200621,-0.00429482, 0.00525732,-0.00410314,-0.00450383, 0.00792112
  , 0.00348008, 0.01214422, 0.0054459 ,-0.00050335, 0.00562416, 0.0028403
  , 0.00742503, 0.01133381,-0.01024828,-0.00731866, 0.00328829,-0.00494739
  , 0.0006761 , 0.00388281],
  [ 8.82213317e-03 , 4.31704216e-03 ,-4.86828239e-03 ,-6.57158862e-05
  ,-4.25254557e-03 ,-7.04788167e-03 ,-1.07084018e-02 , 4.25576733e-03
  ,-6.13082782e-03 ,-1.04779166e-02 , 7.28204097e-03 , 1.09702099e-02
  , 4.70284510e-03 , 1.66104862e-03 , 4.80178362e-03 , 7.20029724e-03
  , 5.34984795e-03 , 2.68943302e-03 ,-9.39927145e-03 ,-8.99856251e-03],
  [-0.00795559, 0.00559293, 0.00334711,-0.00272471, 0.00640333, 0.00327345
  , 0.00447299,-0.01203626, 0.009748  , 0.00600572,-0.0039963 , 0.00400222
  , 0.0043554 , 0.00417211, 0.01208282,-0.00022783, 0.0039221 , 0.01035054
  ,-0.01081022, 0.01178287],
  [ 0.00289327, 0.00336533,-0.00754925, 0.00675789, 0.00579405,-0.00567945
  , 0.00849751, 0.00205789,-0.01052873,-0.00044763,-0.0075924 ,-0.00930875
  , 0.00420149, 0.00953514, 0.00389065, 0.00380556, 0.00128752, 0.00502468
  ,-0.00405938,-0.00232787],
  [-0.00066374,-0.00337095, 0.00015353,-0.00295593,-0.00110554, 0.0069674
  ,-0.01230291, 0.00314067, 0.00192016,-0.00661965, 0.0046424 , 0.01195767
  , 0.00460512,-0.00924398,-0.00813963,-0.00371661,-0.00176077,-0.00955278
  , 0.00691579,-0.00960415],
  [-0.00304175, 0.01228213, 0.01011782,-0.00158962,-0.00259987,-0.01136425
  ,-0.00895158,-0.00085318, 0.00176837,-0.00752886,-0.00784323,-0.0007471
  , 0.01058403, 0.00231221,-0.00705682, 0.00078058, 0.00160885,-0.00134448
  ,-0.01172162, 0.01007056],
  [ 0.00280764, 0.00095453, 0.00408113,-0.00838172, 0.00707021,-0.00763927
  ,-0.00616842, 0.01059211, 0.00495065,-0.00186308, 0.00850648, 0.00599575
  , 0.00595653,-0.00985543,-0.00712822, 0.00811053,-0.00633931, 0.00803064
  ,-0.00641676, 0.0037573 ],
  [-0.00315868,-0.00820077,-0.00051802,-0.00124307,-0.00337273,-0.00706461
  ,-0.00269327,-0.00714946,-0.00224478, 0.010891  ,-0.01069541, 0.00079761
  , 0.01188484, 0.00401949, 0.00998841, 0.00967187,-0.00534798, 0.01081457
  , 0.00483048, 0.01148439]
  ]


layers :: [Layer]
layers = [
    ConvLayer 3 fc1
  , ActLayer relu
  , MaxPoolLayer 2
  , ConvLayer 2 fc2
  , ActLayer relu
  , MaxPoolLayer 2
  , FlattenLayer 2 2
  , FullConnLayer (transpose ff1)
  ]

main :: IO ()
main = do
    putStrLn ("X=\n" ++ (show x))
    putStrLn ("FF1=\n" ++ (show ff1))
    let
      rls = tail $ map reverseLayer $ reverse layers
      im  = fst $ head x
      op  = forwardProp layers [im]
    putStrLn ("OPLEN=\n" ++ (show $ length op))
    putStrLn ("OP=\n" ++ (show $ head op))
