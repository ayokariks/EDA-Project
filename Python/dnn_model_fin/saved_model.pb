??
??
B
AssignVariableOp
resource
value"dtype"
dtypetype?
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
8
Const
output"dtype"
valuetensor"
dtypetype
.
Identity

input"T
output"T"	
Ttype
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
>
Maximum
x"T
y"T
z"T"
Ttype:
2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(?

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetype?
@
RealDiv
x"T
y"T
z"T"
Ttype:
2	
E
Relu
features"T
activations"T"
Ttype:
2	
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
?
Select
	condition

t"T
e"T
output"T"	
Ttype
H
ShardedFilename
basename	
shard

num_shards
filename
-
Sqrt
x"T
y"T"
Ttype:

2
?
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ??
@
StaticRegexFullMatch	
input

output
"
patternstring
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
<
Sub
x"T
y"T
z"T"
Ttype:
2	
?
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 ?"serve*2.7.02v2.7.0-rc1-69-gc256c071bb28??
`
meanVarHandleOp*
_output_shapes
: *
dtype0*
shape:'*
shared_namemean
Y
mean/Read/ReadVariableOpReadVariableOpmean*
_output_shapes
:'*
dtype0
h
varianceVarHandleOp*
_output_shapes
: *
dtype0*
shape:'*
shared_name
variance
a
variance/Read/ReadVariableOpReadVariableOpvariance*
_output_shapes
:'*
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0	
|
dense_576/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:'*!
shared_namedense_576/kernel
u
$dense_576/kernel/Read/ReadVariableOpReadVariableOpdense_576/kernel*
_output_shapes

:'*
dtype0
t
dense_576/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_576/bias
m
"dense_576/bias/Read/ReadVariableOpReadVariableOpdense_576/bias*
_output_shapes
:*
dtype0
|
dense_577/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*!
shared_namedense_577/kernel
u
$dense_577/kernel/Read/ReadVariableOpReadVariableOpdense_577/kernel*
_output_shapes

:*
dtype0
t
dense_577/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_577/bias
m
"dense_577/bias/Read/ReadVariableOpReadVariableOpdense_577/bias*
_output_shapes
:*
dtype0
|
dense_578/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*!
shared_namedense_578/kernel
u
$dense_578/kernel/Read/ReadVariableOpReadVariableOpdense_578/kernel*
_output_shapes

:*
dtype0
t
dense_578/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_578/bias
m
"dense_578/bias/Read/ReadVariableOpReadVariableOpdense_578/bias*
_output_shapes
:*
dtype0
f
	Adam/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_name	Adam/iter
_
Adam/iter/Read/ReadVariableOpReadVariableOp	Adam/iter*
_output_shapes
: *
dtype0	
j
Adam/beta_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_1
c
Adam/beta_1/Read/ReadVariableOpReadVariableOpAdam/beta_1*
_output_shapes
: *
dtype0
j
Adam/beta_2VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_2
c
Adam/beta_2/Read/ReadVariableOpReadVariableOpAdam/beta_2*
_output_shapes
: *
dtype0
h

Adam/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name
Adam/decay
a
Adam/decay/Read/ReadVariableOpReadVariableOp
Adam/decay*
_output_shapes
: *
dtype0
x
Adam/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *#
shared_nameAdam/learning_rate
q
&Adam/learning_rate/Read/ReadVariableOpReadVariableOpAdam/learning_rate*
_output_shapes
: *
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
b
count_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_1
[
count_1/Read/ReadVariableOpReadVariableOpcount_1*
_output_shapes
: *
dtype0
b
total_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_1
[
total_1/Read/ReadVariableOpReadVariableOptotal_1*
_output_shapes
: *
dtype0
b
count_2VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_2
[
count_2/Read/ReadVariableOpReadVariableOpcount_2*
_output_shapes
: *
dtype0
b
total_2VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_2
[
total_2/Read/ReadVariableOpReadVariableOptotal_2*
_output_shapes
: *
dtype0
b
count_3VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_3
[
count_3/Read/ReadVariableOpReadVariableOpcount_3*
_output_shapes
: *
dtype0
b
total_3VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_3
[
total_3/Read/ReadVariableOpReadVariableOptotal_3*
_output_shapes
: *
dtype0
b
count_4VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_4
[
count_4/Read/ReadVariableOpReadVariableOpcount_4*
_output_shapes
: *
dtype0
?
Adam/dense_576/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:'*(
shared_nameAdam/dense_576/kernel/m
?
+Adam/dense_576/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_576/kernel/m*
_output_shapes

:'*
dtype0
?
Adam/dense_576/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*&
shared_nameAdam/dense_576/bias/m
{
)Adam/dense_576/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_576/bias/m*
_output_shapes
:*
dtype0
?
Adam/dense_577/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*(
shared_nameAdam/dense_577/kernel/m
?
+Adam/dense_577/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_577/kernel/m*
_output_shapes

:*
dtype0
?
Adam/dense_577/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*&
shared_nameAdam/dense_577/bias/m
{
)Adam/dense_577/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_577/bias/m*
_output_shapes
:*
dtype0
?
Adam/dense_578/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*(
shared_nameAdam/dense_578/kernel/m
?
+Adam/dense_578/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_578/kernel/m*
_output_shapes

:*
dtype0
?
Adam/dense_578/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*&
shared_nameAdam/dense_578/bias/m
{
)Adam/dense_578/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_578/bias/m*
_output_shapes
:*
dtype0
?
Adam/dense_576/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:'*(
shared_nameAdam/dense_576/kernel/v
?
+Adam/dense_576/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_576/kernel/v*
_output_shapes

:'*
dtype0
?
Adam/dense_576/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*&
shared_nameAdam/dense_576/bias/v
{
)Adam/dense_576/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_576/bias/v*
_output_shapes
:*
dtype0
?
Adam/dense_577/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*(
shared_nameAdam/dense_577/kernel/v
?
+Adam/dense_577/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_577/kernel/v*
_output_shapes

:*
dtype0
?
Adam/dense_577/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*&
shared_nameAdam/dense_577/bias/v
{
)Adam/dense_577/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_577/bias/v*
_output_shapes
:*
dtype0
?
Adam/dense_578/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*(
shared_nameAdam/dense_578/kernel/v
?
+Adam/dense_578/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_578/kernel/v*
_output_shapes

:*
dtype0
?
Adam/dense_578/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*&
shared_nameAdam/dense_578/bias/v
{
)Adam/dense_578/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_578/bias/v*
_output_shapes
:*
dtype0
?
ConstConst*
_output_shapes

:'*
dtype0*?
value?B?'"????@w6?@+y@i??B?.@?$y??e???A?|:@??VB??L??>p?y>???>???>???Ѽ?>#?>t?>??>lJ?>?U4>=?F&???D>??=S?-=?R	=???>7?@<g?9??L?>???'? >	U?>%?h=?w>`?=w?;>
?
Const_1Const*
_output_shapes

:'*
dtype0*?
value?B?'"???A?x???j??KBUD??>=??<??j>?AVe????B?X?W̄k>??<>?Gu>P?|>P?|>?Vq>??>	?~>??|>6?W>?>wR?=?i> ?>???=0D&=??=[|F>?M><?L>??P>??w>	??=??>j?[=?n;>=?@>

NoOpNoOp
?-
Const_2Const"/device:CPU:0*
_output_shapes
: *
dtype0*?-
value?,B?, B?,
?
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
layer_with_weights-2
layer-2
layer_with_weights-3
layer-3
	optimizer
	variables
trainable_variables
regularization_losses
		keras_api


signatures
?

_keep_axis
_reduce_axis
_reduce_axis_mask
_broadcast_shape
mean

adapt_mean
variance
adapt_variance
	count
	keras_api
h

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
h

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
h

kernel
 bias
!	variables
"trainable_variables
#regularization_losses
$	keras_api
?
%iter

&beta_1

'beta_2
	(decay
)learning_ratemTmUmVmWmX mYvZv[v\v]v^ v_
?
0
1
2
3
4
5
6
7
 8
*
0
1
2
3
4
 5
 
?
*non_trainable_variables

+layers
,metrics
-layer_regularization_losses
.layer_metrics
	variables
trainable_variables
regularization_losses
 
 
 
 
 
NL
VARIABLE_VALUEmean4layer_with_weights-0/mean/.ATTRIBUTES/VARIABLE_VALUE
VT
VARIABLE_VALUEvariance8layer_with_weights-0/variance/.ATTRIBUTES/VARIABLE_VALUE
PN
VARIABLE_VALUEcount5layer_with_weights-0/count/.ATTRIBUTES/VARIABLE_VALUE
 
\Z
VARIABLE_VALUEdense_576/kernel6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUE
XV
VARIABLE_VALUEdense_576/bias4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
?
/non_trainable_variables

0layers
1metrics
2layer_regularization_losses
3layer_metrics
	variables
trainable_variables
regularization_losses
\Z
VARIABLE_VALUEdense_577/kernel6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUE
XV
VARIABLE_VALUEdense_577/bias4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
?
4non_trainable_variables

5layers
6metrics
7layer_regularization_losses
8layer_metrics
	variables
trainable_variables
regularization_losses
\Z
VARIABLE_VALUEdense_578/kernel6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUE
XV
VARIABLE_VALUEdense_578/bias4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUE

0
 1

0
 1
 
?
9non_trainable_variables

:layers
;metrics
<layer_regularization_losses
=layer_metrics
!	variables
"trainable_variables
#regularization_losses
HF
VARIABLE_VALUE	Adam/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE
LJ
VARIABLE_VALUEAdam/beta_1+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUE
LJ
VARIABLE_VALUEAdam/beta_2+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUE
JH
VARIABLE_VALUE
Adam/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE
ZX
VARIABLE_VALUEAdam/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE

0
1
2

0
1
2
3

>0
?1
@2
A3
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
4
	Btotal
	Ccount
D	variables
E	keras_api
4
	Ftotal
	Gcount
H	variables
I	keras_api
D
	Jtotal
	Kcount
L
_fn_kwargs
M	variables
N	keras_api
D
	Ototal
	Pcount
Q
_fn_kwargs
R	variables
S	keras_api
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_14keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

B0
C1

D	variables
QO
VARIABLE_VALUEtotal_14keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_24keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUE

F0
G1

H	variables
QO
VARIABLE_VALUEtotal_24keras_api/metrics/2/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_34keras_api/metrics/2/count/.ATTRIBUTES/VARIABLE_VALUE
 

J0
K1

M	variables
QO
VARIABLE_VALUEtotal_34keras_api/metrics/3/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_44keras_api/metrics/3/count/.ATTRIBUTES/VARIABLE_VALUE
 

O0
P1

R	variables
}
VARIABLE_VALUEAdam/dense_576/kernel/mRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUEAdam/dense_576/bias/mPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
}
VARIABLE_VALUEAdam/dense_577/kernel/mRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUEAdam/dense_577/bias/mPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
}
VARIABLE_VALUEAdam/dense_578/kernel/mRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUEAdam/dense_578/bias/mPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
}
VARIABLE_VALUEAdam/dense_576/kernel/vRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUEAdam/dense_576/bias/vPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
}
VARIABLE_VALUEAdam/dense_577/kernel/vRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUEAdam/dense_577/bias/vPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
}
VARIABLE_VALUEAdam/dense_578/kernel/vRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUEAdam/dense_578/bias/vPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
?
%serving_default_normalization_2_inputPlaceholder*0
_output_shapes
:??????????????????*
dtype0*%
shape:??????????????????
?
StatefulPartitionedCallStatefulPartitionedCall%serving_default_normalization_2_inputConstConst_1dense_576/kerneldense_576/biasdense_577/kerneldense_577/biasdense_578/kerneldense_578/bias*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *0
f+R)
'__inference_signature_wrapper_167186224
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
?
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filenamemean/Read/ReadVariableOpvariance/Read/ReadVariableOpcount/Read/ReadVariableOp$dense_576/kernel/Read/ReadVariableOp"dense_576/bias/Read/ReadVariableOp$dense_577/kernel/Read/ReadVariableOp"dense_577/bias/Read/ReadVariableOp$dense_578/kernel/Read/ReadVariableOp"dense_578/bias/Read/ReadVariableOpAdam/iter/Read/ReadVariableOpAdam/beta_1/Read/ReadVariableOpAdam/beta_2/Read/ReadVariableOpAdam/decay/Read/ReadVariableOp&Adam/learning_rate/Read/ReadVariableOptotal/Read/ReadVariableOpcount_1/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_2/Read/ReadVariableOptotal_2/Read/ReadVariableOpcount_3/Read/ReadVariableOptotal_3/Read/ReadVariableOpcount_4/Read/ReadVariableOp+Adam/dense_576/kernel/m/Read/ReadVariableOp)Adam/dense_576/bias/m/Read/ReadVariableOp+Adam/dense_577/kernel/m/Read/ReadVariableOp)Adam/dense_577/bias/m/Read/ReadVariableOp+Adam/dense_578/kernel/m/Read/ReadVariableOp)Adam/dense_578/bias/m/Read/ReadVariableOp+Adam/dense_576/kernel/v/Read/ReadVariableOp)Adam/dense_576/bias/v/Read/ReadVariableOp+Adam/dense_577/kernel/v/Read/ReadVariableOp)Adam/dense_577/bias/v/Read/ReadVariableOp+Adam/dense_578/kernel/v/Read/ReadVariableOp)Adam/dense_578/bias/v/Read/ReadVariableOpConst_2*/
Tin(
&2$		*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *+
f&R$
"__inference__traced_save_167186560
?
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamemeanvariancecountdense_576/kerneldense_576/biasdense_577/kerneldense_577/biasdense_578/kerneldense_578/bias	Adam/iterAdam/beta_1Adam/beta_2
Adam/decayAdam/learning_ratetotalcount_1total_1count_2total_2count_3total_3count_4Adam/dense_576/kernel/mAdam/dense_576/bias/mAdam/dense_577/kernel/mAdam/dense_577/bias/mAdam/dense_578/kernel/mAdam/dense_578/bias/mAdam/dense_576/kernel/vAdam/dense_576/bias/vAdam/dense_577/kernel/vAdam/dense_577/bias/vAdam/dense_578/kernel/vAdam/dense_578/bias/v*.
Tin'
%2#*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *.
f)R'
%__inference__traced_restore_167186672??
?
?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186005

inputs
normalization_2_sub_y
normalization_2_sqrt_x%
dense_576_167185966:'!
dense_576_167185968:%
dense_577_167185983:!
dense_577_167185985:%
dense_578_167185999:!
dense_578_167186001:
identity??!dense_576/StatefulPartitionedCall?!dense_577/StatefulPartitionedCall?!dense_578/StatefulPartitionedCallk
normalization_2/subSubinputsnormalization_2_sub_y*
T0*'
_output_shapes
:?????????']
normalization_2/SqrtSqrtnormalization_2_sqrt_x*
T0*
_output_shapes

:'^
normalization_2/Maximum/yConst*
_output_shapes
: *
dtype0*
valueB
 *???3?
normalization_2/MaximumMaximumnormalization_2/Sqrt:y:0"normalization_2/Maximum/y:output:0*
T0*
_output_shapes

:'?
normalization_2/truedivRealDivnormalization_2/sub:z:0normalization_2/Maximum:z:0*
T0*'
_output_shapes
:?????????'?
!dense_576/StatefulPartitionedCallStatefulPartitionedCallnormalization_2/truediv:z:0dense_576_167185966dense_576_167185968*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_576_layer_call_and_return_conditional_losses_167185965?
!dense_577/StatefulPartitionedCallStatefulPartitionedCall*dense_576/StatefulPartitionedCall:output:0dense_577_167185983dense_577_167185985*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_577_layer_call_and_return_conditional_losses_167185982?
!dense_578/StatefulPartitionedCallStatefulPartitionedCall*dense_577/StatefulPartitionedCall:output:0dense_578_167185999dense_578_167186001*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_578_layer_call_and_return_conditional_losses_167185998y
IdentityIdentity*dense_578/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp"^dense_576/StatefulPartitionedCall"^dense_577/StatefulPartitionedCall"^dense_578/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 2F
!dense_576/StatefulPartitionedCall!dense_576/StatefulPartitionedCall2F
!dense_577/StatefulPartitionedCall!dense_577/StatefulPartitionedCall2F
!dense_578/StatefulPartitionedCall!dense_578/StatefulPartitionedCall:X T
0
_output_shapes
:??????????????????
 
_user_specified_nameinputs:$ 

_output_shapes

:':$ 

_output_shapes

:'
?	
?
H__inference_dense_578_layer_call_and_return_conditional_losses_167186433

inputs0
matmul_readvariableop_resource:-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????_
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
-__inference_dense_576_layer_call_fn_167186383

inputs
unknown:'
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_576_layer_call_and_return_conditional_losses_167185965o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????': : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????'
 
_user_specified_nameinputs
?

?
H__inference_dense_577_layer_call_and_return_conditional_losses_167186414

inputs0
matmul_readvariableop_resource:-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????P
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????a
IdentityIdentityRelu:activations:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?E
?
"__inference__traced_save_167186560
file_prefix#
savev2_mean_read_readvariableop'
#savev2_variance_read_readvariableop$
 savev2_count_read_readvariableop	/
+savev2_dense_576_kernel_read_readvariableop-
)savev2_dense_576_bias_read_readvariableop/
+savev2_dense_577_kernel_read_readvariableop-
)savev2_dense_577_bias_read_readvariableop/
+savev2_dense_578_kernel_read_readvariableop-
)savev2_dense_578_bias_read_readvariableop(
$savev2_adam_iter_read_readvariableop	*
&savev2_adam_beta_1_read_readvariableop*
&savev2_adam_beta_2_read_readvariableop)
%savev2_adam_decay_read_readvariableop1
-savev2_adam_learning_rate_read_readvariableop$
 savev2_total_read_readvariableop&
"savev2_count_1_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_2_read_readvariableop&
"savev2_total_2_read_readvariableop&
"savev2_count_3_read_readvariableop&
"savev2_total_3_read_readvariableop&
"savev2_count_4_read_readvariableop6
2savev2_adam_dense_576_kernel_m_read_readvariableop4
0savev2_adam_dense_576_bias_m_read_readvariableop6
2savev2_adam_dense_577_kernel_m_read_readvariableop4
0savev2_adam_dense_577_bias_m_read_readvariableop6
2savev2_adam_dense_578_kernel_m_read_readvariableop4
0savev2_adam_dense_578_bias_m_read_readvariableop6
2savev2_adam_dense_576_kernel_v_read_readvariableop4
0savev2_adam_dense_576_bias_v_read_readvariableop6
2savev2_adam_dense_577_kernel_v_read_readvariableop4
0savev2_adam_dense_577_bias_v_read_readvariableop6
2savev2_adam_dense_578_kernel_v_read_readvariableop4
0savev2_adam_dense_578_bias_v_read_readvariableop
savev2_const_2

identity_1??MergeV2Checkpointsw
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*Z
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.parta
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part?
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: f

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: L

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :f
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : ?
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: ?
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:#*
dtype0*?
value?B?#B4layer_with_weights-0/mean/.ATTRIBUTES/VARIABLE_VALUEB8layer_with_weights-0/variance/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-0/count/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/2/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/2/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/3/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/3/count/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH?
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:#*
dtype0*Y
valuePBN#B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B ?
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0savev2_mean_read_readvariableop#savev2_variance_read_readvariableop savev2_count_read_readvariableop+savev2_dense_576_kernel_read_readvariableop)savev2_dense_576_bias_read_readvariableop+savev2_dense_577_kernel_read_readvariableop)savev2_dense_577_bias_read_readvariableop+savev2_dense_578_kernel_read_readvariableop)savev2_dense_578_bias_read_readvariableop$savev2_adam_iter_read_readvariableop&savev2_adam_beta_1_read_readvariableop&savev2_adam_beta_2_read_readvariableop%savev2_adam_decay_read_readvariableop-savev2_adam_learning_rate_read_readvariableop savev2_total_read_readvariableop"savev2_count_1_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_2_read_readvariableop"savev2_total_2_read_readvariableop"savev2_count_3_read_readvariableop"savev2_total_3_read_readvariableop"savev2_count_4_read_readvariableop2savev2_adam_dense_576_kernel_m_read_readvariableop0savev2_adam_dense_576_bias_m_read_readvariableop2savev2_adam_dense_577_kernel_m_read_readvariableop0savev2_adam_dense_577_bias_m_read_readvariableop2savev2_adam_dense_578_kernel_m_read_readvariableop0savev2_adam_dense_578_bias_m_read_readvariableop2savev2_adam_dense_576_kernel_v_read_readvariableop0savev2_adam_dense_576_bias_v_read_readvariableop2savev2_adam_dense_577_kernel_v_read_readvariableop0savev2_adam_dense_577_bias_v_read_readvariableop2savev2_adam_dense_578_kernel_v_read_readvariableop0savev2_adam_dense_578_bias_v_read_readvariableopsavev2_const_2"/device:CPU:0*
_output_shapes
 *1
dtypes'
%2#		?
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:?
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 f
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: Q

Identity_1IdentityIdentity:output:0^NoOp*
T0*
_output_shapes
: [
NoOpNoOp^MergeV2Checkpoints*"
_acd_function_control_output(*
_output_shapes
 "!

identity_1Identity_1:output:0*?
_input_shapes?
?: :':': :':::::: : : : : : : : : : : : : :'::::::':::::: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix: 

_output_shapes
:': 

_output_shapes
:':

_output_shapes
: :$ 

_output_shapes

:': 

_output_shapes
::$ 

_output_shapes

:: 

_output_shapes
::$ 

_output_shapes

:: 	

_output_shapes
::


_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:': 

_output_shapes
::$ 

_output_shapes

:: 

_output_shapes
::$ 

_output_shapes

:: 

_output_shapes
::$ 

_output_shapes

:': 

_output_shapes
::$ 

_output_shapes

::  

_output_shapes
::$! 

_output_shapes

:: "

_output_shapes
::#

_output_shapes
: 
?'
?
 __inference_adapt_step_167186374
iterator

iterator_1%
add_readvariableop_resource:	 %
readvariableop_resource:''
readvariableop_2_resource:'??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_2?IteratorGetNext?ReadVariableOp?ReadVariableOp_1?ReadVariableOp_2?add/ReadVariableOp?
IteratorGetNextIteratorGetNextiterator*
_class
loc:@iterator*'
_output_shapes
:?????????'*&
output_shapes
:?????????'*
output_types
2h
moments/mean/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: ?
moments/meanMeanIteratorGetNext:components:0'moments/mean/reduction_indices:output:0*
T0*
_output_shapes

:'*
	keep_dims(d
moments/StopGradientStopGradientmoments/mean:output:0*
T0*
_output_shapes

:'?
moments/SquaredDifferenceSquaredDifferenceIteratorGetNext:components:0moments/StopGradient:output:0*
T0*'
_output_shapes
:?????????'l
"moments/variance/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: ?
moments/varianceMeanmoments/SquaredDifference:z:0+moments/variance/reduction_indices:output:0*
T0*
_output_shapes

:'*
	keep_dims(m
moments/SqueezeSqueezemoments/mean:output:0*
T0*
_output_shapes
:'*
squeeze_dims
 s
moments/Squeeze_1Squeezemoments/variance:output:0*
T0*
_output_shapes
:'*
squeeze_dims
 a
ShapeShapeIteratorGetNext:components:0*
T0*
_output_shapes
:*
out_type0	Z
GatherV2/indicesConst*
_output_shapes
:*
dtype0*
valueB: O
GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
GatherV2GatherV2Shape:output:0GatherV2/indices:output:0GatherV2/axis:output:0*
Taxis0*
Tindices0*
Tparams0	*
_output_shapes
:O
ConstConst*
_output_shapes
:*
dtype0*
valueB: P
ProdProdGatherV2:output:0Const:output:0*
T0	*
_output_shapes
: f
add/ReadVariableOpReadVariableOpadd_readvariableop_resource*
_output_shapes
: *
dtype0	X
addAddV2Prod:output:0add/ReadVariableOp:value:0*
T0	*
_output_shapes
: K
CastCastProd:output:0*

DstT0*

SrcT0	*
_output_shapes
: G
Cast_1Castadd:z:0*

DstT0*

SrcT0	*
_output_shapes
: I
truedivRealDivCast:y:0
Cast_1:y:0*
T0*
_output_shapes
: J
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ??H
subSubsub/x:output:0truediv:z:0*
T0*
_output_shapes
: b
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes
:'*
dtype0P
mulMulReadVariableOp:value:0sub:z:0*
T0*
_output_shapes
:'X
mul_1Mulmoments/Squeeze:output:0truediv:z:0*
T0*
_output_shapes
:'G
add_1AddV2mul:z:0	mul_1:z:0*
T0*
_output_shapes
:'d
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes
:'*
dtype0V
sub_1SubReadVariableOp_1:value:0	add_1:z:0*
T0*
_output_shapes
:'J
pow/yConst*
_output_shapes
: *
dtype0*
valueB
 *   @J
powPow	sub_1:z:0pow/y:output:0*
T0*
_output_shapes
:'f
ReadVariableOp_2ReadVariableOpreadvariableop_2_resource*
_output_shapes
:'*
dtype0V
add_2AddV2ReadVariableOp_2:value:0pow:z:0*
T0*
_output_shapes
:'E
mul_2Mul	add_2:z:0sub:z:0*
T0*
_output_shapes
:'V
sub_2Submoments/Squeeze:output:0	add_1:z:0*
T0*
_output_shapes
:'L
pow_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *   @N
pow_1Pow	sub_2:z:0pow_1/y:output:0*
T0*
_output_shapes
:'Z
add_3AddV2moments/Squeeze_1:output:0	pow_1:z:0*
T0*
_output_shapes
:'I
mul_3Mul	add_3:z:0truediv:z:0*
T0*
_output_shapes
:'I
add_4AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:'?
AssignVariableOpAssignVariableOpreadvariableop_resource	add_1:z:0^ReadVariableOp^ReadVariableOp_1*
_output_shapes
 *
dtype0?
AssignVariableOp_1AssignVariableOpreadvariableop_2_resource	add_4:z:0^ReadVariableOp_2*
_output_shapes
 *
dtype0?
AssignVariableOp_2AssignVariableOpadd_readvariableop_resourceadd:z:0^add/ReadVariableOp*
_output_shapes
 *
dtype0	*(
_construction_contextkEagerRuntime*
_input_shapes

: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12(
AssignVariableOp_2AssignVariableOp_22"
IteratorGetNextIteratorGetNext2 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12$
ReadVariableOp_2ReadVariableOp_22(
add/ReadVariableOpadd/ReadVariableOp:( $
"
_user_specified_name
iterator:@<

_output_shapes
: 
"
_user_specified_name
iterator
?
?
-__inference_dense_578_layer_call_fn_167186423

inputs
unknown:
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_578_layer_call_and_return_conditional_losses_167185998o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
2__inference_sequential_192_layer_call_fn_167186245

inputs
unknown
	unknown_0
	unknown_1:'
	unknown_2:
	unknown_3:
	unknown_4:
	unknown_5:
	unknown_6:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186005o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:X T
0
_output_shapes
:??????????????????
 
_user_specified_nameinputs:$ 

_output_shapes

:':$ 

_output_shapes

:'
?

?
H__inference_dense_576_layer_call_and_return_conditional_losses_167185965

inputs0
matmul_readvariableop_resource:'-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:'*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????P
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????a
IdentityIdentityRelu:activations:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????': : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????'
 
_user_specified_nameinputs
?"
?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186328

inputs
normalization_2_sub_y
normalization_2_sqrt_x:
(dense_576_matmul_readvariableop_resource:'7
)dense_576_biasadd_readvariableop_resource::
(dense_577_matmul_readvariableop_resource:7
)dense_577_biasadd_readvariableop_resource::
(dense_578_matmul_readvariableop_resource:7
)dense_578_biasadd_readvariableop_resource:
identity?? dense_576/BiasAdd/ReadVariableOp?dense_576/MatMul/ReadVariableOp? dense_577/BiasAdd/ReadVariableOp?dense_577/MatMul/ReadVariableOp? dense_578/BiasAdd/ReadVariableOp?dense_578/MatMul/ReadVariableOpk
normalization_2/subSubinputsnormalization_2_sub_y*
T0*'
_output_shapes
:?????????']
normalization_2/SqrtSqrtnormalization_2_sqrt_x*
T0*
_output_shapes

:'^
normalization_2/Maximum/yConst*
_output_shapes
: *
dtype0*
valueB
 *???3?
normalization_2/MaximumMaximumnormalization_2/Sqrt:y:0"normalization_2/Maximum/y:output:0*
T0*
_output_shapes

:'?
normalization_2/truedivRealDivnormalization_2/sub:z:0normalization_2/Maximum:z:0*
T0*'
_output_shapes
:?????????'?
dense_576/MatMul/ReadVariableOpReadVariableOp(dense_576_matmul_readvariableop_resource*
_output_shapes

:'*
dtype0?
dense_576/MatMulMatMulnormalization_2/truediv:z:0'dense_576/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
 dense_576/BiasAdd/ReadVariableOpReadVariableOp)dense_576_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_576/BiasAddBiasAdddense_576/MatMul:product:0(dense_576/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????d
dense_576/ReluReludense_576/BiasAdd:output:0*
T0*'
_output_shapes
:??????????
dense_577/MatMul/ReadVariableOpReadVariableOp(dense_577_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
dense_577/MatMulMatMuldense_576/Relu:activations:0'dense_577/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
 dense_577/BiasAdd/ReadVariableOpReadVariableOp)dense_577_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_577/BiasAddBiasAdddense_577/MatMul:product:0(dense_577/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????d
dense_577/ReluReludense_577/BiasAdd:output:0*
T0*'
_output_shapes
:??????????
dense_578/MatMul/ReadVariableOpReadVariableOp(dense_578_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
dense_578/MatMulMatMuldense_577/Relu:activations:0'dense_578/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
 dense_578/BiasAdd/ReadVariableOpReadVariableOp)dense_578_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_578/BiasAddBiasAdddense_578/MatMul:product:0(dense_578/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????i
IdentityIdentitydense_578/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp!^dense_576/BiasAdd/ReadVariableOp ^dense_576/MatMul/ReadVariableOp!^dense_577/BiasAdd/ReadVariableOp ^dense_577/MatMul/ReadVariableOp!^dense_578/BiasAdd/ReadVariableOp ^dense_578/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 2D
 dense_576/BiasAdd/ReadVariableOp dense_576/BiasAdd/ReadVariableOp2B
dense_576/MatMul/ReadVariableOpdense_576/MatMul/ReadVariableOp2D
 dense_577/BiasAdd/ReadVariableOp dense_577/BiasAdd/ReadVariableOp2B
dense_577/MatMul/ReadVariableOpdense_577/MatMul/ReadVariableOp2D
 dense_578/BiasAdd/ReadVariableOp dense_578/BiasAdd/ReadVariableOp2B
dense_578/MatMul/ReadVariableOpdense_578/MatMul/ReadVariableOp:X T
0
_output_shapes
:??????????????????
 
_user_specified_nameinputs:$ 

_output_shapes

:':$ 

_output_shapes

:'
?

?
H__inference_dense_576_layer_call_and_return_conditional_losses_167186394

inputs0
matmul_readvariableop_resource:'-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:'*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????P
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????a
IdentityIdentityRelu:activations:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????': : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????'
 
_user_specified_nameinputs
?
?
-__inference_dense_577_layer_call_fn_167186403

inputs
unknown:
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_577_layer_call_and_return_conditional_losses_167185982o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?	
?
H__inference_dense_578_layer_call_and_return_conditional_losses_167185998

inputs0
matmul_readvariableop_resource:-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????_
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
2__inference_sequential_192_layer_call_fn_167186024
normalization_2_input
unknown
	unknown_0
	unknown_1:'
	unknown_2:
	unknown_3:
	unknown_4:
	unknown_5:
	unknown_6:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallnormalization_2_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186005o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:g c
0
_output_shapes
:??????????????????
/
_user_specified_namenormalization_2_input:$ 

_output_shapes

:':$ 

_output_shapes

:'
?

?
H__inference_dense_577_layer_call_and_return_conditional_losses_167185982

inputs0
matmul_readvariableop_resource:-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????P
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????a
IdentityIdentityRelu:activations:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
2__inference_sequential_192_layer_call_fn_167186143
normalization_2_input
unknown
	unknown_0
	unknown_1:'
	unknown_2:
	unknown_3:
	unknown_4:
	unknown_5:
	unknown_6:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallnormalization_2_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186103o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:g c
0
_output_shapes
:??????????????????
/
_user_specified_namenormalization_2_input:$ 

_output_shapes

:':$ 

_output_shapes

:'
?

?
2__inference_sequential_192_layer_call_fn_167186266

inputs
unknown
	unknown_0
	unknown_1:'
	unknown_2:
	unknown_3:
	unknown_4:
	unknown_5:
	unknown_6:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186103o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:X T
0
_output_shapes
:??????????????????
 
_user_specified_nameinputs:$ 

_output_shapes

:':$ 

_output_shapes

:'
?+
?
$__inference__wrapped_model_167185940
normalization_2_input(
$sequential_192_normalization_2_sub_y)
%sequential_192_normalization_2_sqrt_xI
7sequential_192_dense_576_matmul_readvariableop_resource:'F
8sequential_192_dense_576_biasadd_readvariableop_resource:I
7sequential_192_dense_577_matmul_readvariableop_resource:F
8sequential_192_dense_577_biasadd_readvariableop_resource:I
7sequential_192_dense_578_matmul_readvariableop_resource:F
8sequential_192_dense_578_biasadd_readvariableop_resource:
identity??/sequential_192/dense_576/BiasAdd/ReadVariableOp?.sequential_192/dense_576/MatMul/ReadVariableOp?/sequential_192/dense_577/BiasAdd/ReadVariableOp?.sequential_192/dense_577/MatMul/ReadVariableOp?/sequential_192/dense_578/BiasAdd/ReadVariableOp?.sequential_192/dense_578/MatMul/ReadVariableOp?
"sequential_192/normalization_2/subSubnormalization_2_input$sequential_192_normalization_2_sub_y*
T0*'
_output_shapes
:?????????'{
#sequential_192/normalization_2/SqrtSqrt%sequential_192_normalization_2_sqrt_x*
T0*
_output_shapes

:'m
(sequential_192/normalization_2/Maximum/yConst*
_output_shapes
: *
dtype0*
valueB
 *???3?
&sequential_192/normalization_2/MaximumMaximum'sequential_192/normalization_2/Sqrt:y:01sequential_192/normalization_2/Maximum/y:output:0*
T0*
_output_shapes

:'?
&sequential_192/normalization_2/truedivRealDiv&sequential_192/normalization_2/sub:z:0*sequential_192/normalization_2/Maximum:z:0*
T0*'
_output_shapes
:?????????'?
.sequential_192/dense_576/MatMul/ReadVariableOpReadVariableOp7sequential_192_dense_576_matmul_readvariableop_resource*
_output_shapes

:'*
dtype0?
sequential_192/dense_576/MatMulMatMul*sequential_192/normalization_2/truediv:z:06sequential_192/dense_576/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
/sequential_192/dense_576/BiasAdd/ReadVariableOpReadVariableOp8sequential_192_dense_576_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
 sequential_192/dense_576/BiasAddBiasAdd)sequential_192/dense_576/MatMul:product:07sequential_192/dense_576/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
sequential_192/dense_576/ReluRelu)sequential_192/dense_576/BiasAdd:output:0*
T0*'
_output_shapes
:??????????
.sequential_192/dense_577/MatMul/ReadVariableOpReadVariableOp7sequential_192_dense_577_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
sequential_192/dense_577/MatMulMatMul+sequential_192/dense_576/Relu:activations:06sequential_192/dense_577/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
/sequential_192/dense_577/BiasAdd/ReadVariableOpReadVariableOp8sequential_192_dense_577_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
 sequential_192/dense_577/BiasAddBiasAdd)sequential_192/dense_577/MatMul:product:07sequential_192/dense_577/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
sequential_192/dense_577/ReluRelu)sequential_192/dense_577/BiasAdd:output:0*
T0*'
_output_shapes
:??????????
.sequential_192/dense_578/MatMul/ReadVariableOpReadVariableOp7sequential_192_dense_578_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
sequential_192/dense_578/MatMulMatMul+sequential_192/dense_577/Relu:activations:06sequential_192/dense_578/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
/sequential_192/dense_578/BiasAdd/ReadVariableOpReadVariableOp8sequential_192_dense_578_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
 sequential_192/dense_578/BiasAddBiasAdd)sequential_192/dense_578/MatMul:product:07sequential_192/dense_578/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????x
IdentityIdentity)sequential_192/dense_578/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp0^sequential_192/dense_576/BiasAdd/ReadVariableOp/^sequential_192/dense_576/MatMul/ReadVariableOp0^sequential_192/dense_577/BiasAdd/ReadVariableOp/^sequential_192/dense_577/MatMul/ReadVariableOp0^sequential_192/dense_578/BiasAdd/ReadVariableOp/^sequential_192/dense_578/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 2b
/sequential_192/dense_576/BiasAdd/ReadVariableOp/sequential_192/dense_576/BiasAdd/ReadVariableOp2`
.sequential_192/dense_576/MatMul/ReadVariableOp.sequential_192/dense_576/MatMul/ReadVariableOp2b
/sequential_192/dense_577/BiasAdd/ReadVariableOp/sequential_192/dense_577/BiasAdd/ReadVariableOp2`
.sequential_192/dense_577/MatMul/ReadVariableOp.sequential_192/dense_577/MatMul/ReadVariableOp2b
/sequential_192/dense_578/BiasAdd/ReadVariableOp/sequential_192/dense_578/BiasAdd/ReadVariableOp2`
.sequential_192/dense_578/MatMul/ReadVariableOp.sequential_192/dense_578/MatMul/ReadVariableOp:g c
0
_output_shapes
:??????????????????
/
_user_specified_namenormalization_2_input:$ 

_output_shapes

:':$ 

_output_shapes

:'
ׅ
?
%__inference__traced_restore_167186672
file_prefix#
assignvariableop_mean:')
assignvariableop_1_variance:'"
assignvariableop_2_count:	 5
#assignvariableop_3_dense_576_kernel:'/
!assignvariableop_4_dense_576_bias:5
#assignvariableop_5_dense_577_kernel:/
!assignvariableop_6_dense_577_bias:5
#assignvariableop_7_dense_578_kernel:/
!assignvariableop_8_dense_578_bias:&
assignvariableop_9_adam_iter:	 )
assignvariableop_10_adam_beta_1: )
assignvariableop_11_adam_beta_2: (
assignvariableop_12_adam_decay: 0
&assignvariableop_13_adam_learning_rate: #
assignvariableop_14_total: %
assignvariableop_15_count_1: %
assignvariableop_16_total_1: %
assignvariableop_17_count_2: %
assignvariableop_18_total_2: %
assignvariableop_19_count_3: %
assignvariableop_20_total_3: %
assignvariableop_21_count_4: =
+assignvariableop_22_adam_dense_576_kernel_m:'7
)assignvariableop_23_adam_dense_576_bias_m:=
+assignvariableop_24_adam_dense_577_kernel_m:7
)assignvariableop_25_adam_dense_577_bias_m:=
+assignvariableop_26_adam_dense_578_kernel_m:7
)assignvariableop_27_adam_dense_578_bias_m:=
+assignvariableop_28_adam_dense_576_kernel_v:'7
)assignvariableop_29_adam_dense_576_bias_v:=
+assignvariableop_30_adam_dense_577_kernel_v:7
)assignvariableop_31_adam_dense_577_bias_v:=
+assignvariableop_32_adam_dense_578_kernel_v:7
)assignvariableop_33_adam_dense_578_bias_v:
identity_35??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_10?AssignVariableOp_11?AssignVariableOp_12?AssignVariableOp_13?AssignVariableOp_14?AssignVariableOp_15?AssignVariableOp_16?AssignVariableOp_17?AssignVariableOp_18?AssignVariableOp_19?AssignVariableOp_2?AssignVariableOp_20?AssignVariableOp_21?AssignVariableOp_22?AssignVariableOp_23?AssignVariableOp_24?AssignVariableOp_25?AssignVariableOp_26?AssignVariableOp_27?AssignVariableOp_28?AssignVariableOp_29?AssignVariableOp_3?AssignVariableOp_30?AssignVariableOp_31?AssignVariableOp_32?AssignVariableOp_33?AssignVariableOp_4?AssignVariableOp_5?AssignVariableOp_6?AssignVariableOp_7?AssignVariableOp_8?AssignVariableOp_9?
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:#*
dtype0*?
value?B?#B4layer_with_weights-0/mean/.ATTRIBUTES/VARIABLE_VALUEB8layer_with_weights-0/variance/.ATTRIBUTES/VARIABLE_VALUEB5layer_with_weights-0/count/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/2/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/2/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/3/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/3/count/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH?
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:#*
dtype0*Y
valuePBN#B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B ?
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*?
_output_shapes?
?:::::::::::::::::::::::::::::::::::*1
dtypes'
%2#		[
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOpAssignVariableOpassignvariableop_meanIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_1AssignVariableOpassignvariableop_1_varianceIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0	*
_output_shapes
:?
AssignVariableOp_2AssignVariableOpassignvariableop_2_countIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	]

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_3AssignVariableOp#assignvariableop_3_dense_576_kernelIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_4AssignVariableOp!assignvariableop_4_dense_576_biasIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_5AssignVariableOp#assignvariableop_5_dense_577_kernelIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_6AssignVariableOp!assignvariableop_6_dense_577_biasIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_7AssignVariableOp#assignvariableop_7_dense_578_kernelIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_8AssignVariableOp!assignvariableop_8_dense_578_biasIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0	*
_output_shapes
:?
AssignVariableOp_9AssignVariableOpassignvariableop_9_adam_iterIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	_
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_10AssignVariableOpassignvariableop_10_adam_beta_1Identity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_11AssignVariableOpassignvariableop_11_adam_beta_2Identity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_12AssignVariableOpassignvariableop_12_adam_decayIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_13AssignVariableOp&assignvariableop_13_adam_learning_rateIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_14AssignVariableOpassignvariableop_14_totalIdentity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_15AssignVariableOpassignvariableop_15_count_1Identity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_16AssignVariableOpassignvariableop_16_total_1Identity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_17AssignVariableOpassignvariableop_17_count_2Identity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_18AssignVariableOpassignvariableop_18_total_2Identity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_19AssignVariableOpassignvariableop_19_count_3Identity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_20AssignVariableOpassignvariableop_20_total_3Identity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_21IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_21AssignVariableOpassignvariableop_21_count_4Identity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_22IdentityRestoreV2:tensors:22"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_22AssignVariableOp+assignvariableop_22_adam_dense_576_kernel_mIdentity_22:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_23IdentityRestoreV2:tensors:23"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_23AssignVariableOp)assignvariableop_23_adam_dense_576_bias_mIdentity_23:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_24IdentityRestoreV2:tensors:24"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_24AssignVariableOp+assignvariableop_24_adam_dense_577_kernel_mIdentity_24:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_25IdentityRestoreV2:tensors:25"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_25AssignVariableOp)assignvariableop_25_adam_dense_577_bias_mIdentity_25:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_26IdentityRestoreV2:tensors:26"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_26AssignVariableOp+assignvariableop_26_adam_dense_578_kernel_mIdentity_26:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_27IdentityRestoreV2:tensors:27"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_27AssignVariableOp)assignvariableop_27_adam_dense_578_bias_mIdentity_27:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_28IdentityRestoreV2:tensors:28"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_28AssignVariableOp+assignvariableop_28_adam_dense_576_kernel_vIdentity_28:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_29IdentityRestoreV2:tensors:29"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_29AssignVariableOp)assignvariableop_29_adam_dense_576_bias_vIdentity_29:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_30IdentityRestoreV2:tensors:30"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_30AssignVariableOp+assignvariableop_30_adam_dense_577_kernel_vIdentity_30:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_31IdentityRestoreV2:tensors:31"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_31AssignVariableOp)assignvariableop_31_adam_dense_577_bias_vIdentity_31:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_32IdentityRestoreV2:tensors:32"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_32AssignVariableOp+assignvariableop_32_adam_dense_578_kernel_vIdentity_32:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_33IdentityRestoreV2:tensors:33"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_33AssignVariableOp)assignvariableop_33_adam_dense_578_bias_vIdentity_33:output:0"/device:CPU:0*
_output_shapes
 *
dtype01
NoOpNoOp"/device:CPU:0*
_output_shapes
 ?
Identity_34Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: W
Identity_35IdentityIdentity_34:output:0^NoOp_1*
T0*
_output_shapes
: ?
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9*"
_acd_function_control_output(*
_output_shapes
 "#
identity_35Identity_35:output:0*Y
_input_shapesH
F: : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202*
AssignVariableOp_21AssignVariableOp_212*
AssignVariableOp_22AssignVariableOp_222*
AssignVariableOp_23AssignVariableOp_232*
AssignVariableOp_24AssignVariableOp_242*
AssignVariableOp_25AssignVariableOp_252*
AssignVariableOp_26AssignVariableOp_262*
AssignVariableOp_27AssignVariableOp_272*
AssignVariableOp_28AssignVariableOp_282*
AssignVariableOp_29AssignVariableOp_292(
AssignVariableOp_3AssignVariableOp_32*
AssignVariableOp_30AssignVariableOp_302*
AssignVariableOp_31AssignVariableOp_312*
AssignVariableOp_32AssignVariableOp_322*
AssignVariableOp_33AssignVariableOp_332(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_9:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix
?
?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186169
normalization_2_input
normalization_2_sub_y
normalization_2_sqrt_x%
dense_576_167186153:'!
dense_576_167186155:%
dense_577_167186158:!
dense_577_167186160:%
dense_578_167186163:!
dense_578_167186165:
identity??!dense_576/StatefulPartitionedCall?!dense_577/StatefulPartitionedCall?!dense_578/StatefulPartitionedCallz
normalization_2/subSubnormalization_2_inputnormalization_2_sub_y*
T0*'
_output_shapes
:?????????']
normalization_2/SqrtSqrtnormalization_2_sqrt_x*
T0*
_output_shapes

:'^
normalization_2/Maximum/yConst*
_output_shapes
: *
dtype0*
valueB
 *???3?
normalization_2/MaximumMaximumnormalization_2/Sqrt:y:0"normalization_2/Maximum/y:output:0*
T0*
_output_shapes

:'?
normalization_2/truedivRealDivnormalization_2/sub:z:0normalization_2/Maximum:z:0*
T0*'
_output_shapes
:?????????'?
!dense_576/StatefulPartitionedCallStatefulPartitionedCallnormalization_2/truediv:z:0dense_576_167186153dense_576_167186155*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_576_layer_call_and_return_conditional_losses_167185965?
!dense_577/StatefulPartitionedCallStatefulPartitionedCall*dense_576/StatefulPartitionedCall:output:0dense_577_167186158dense_577_167186160*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_577_layer_call_and_return_conditional_losses_167185982?
!dense_578/StatefulPartitionedCallStatefulPartitionedCall*dense_577/StatefulPartitionedCall:output:0dense_578_167186163dense_578_167186165*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_578_layer_call_and_return_conditional_losses_167185998y
IdentityIdentity*dense_578/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp"^dense_576/StatefulPartitionedCall"^dense_577/StatefulPartitionedCall"^dense_578/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 2F
!dense_576/StatefulPartitionedCall!dense_576/StatefulPartitionedCall2F
!dense_577/StatefulPartitionedCall!dense_577/StatefulPartitionedCall2F
!dense_578/StatefulPartitionedCall!dense_578/StatefulPartitionedCall:g c
0
_output_shapes
:??????????????????
/
_user_specified_namenormalization_2_input:$ 

_output_shapes

:':$ 

_output_shapes

:'
?
?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186195
normalization_2_input
normalization_2_sub_y
normalization_2_sqrt_x%
dense_576_167186179:'!
dense_576_167186181:%
dense_577_167186184:!
dense_577_167186186:%
dense_578_167186189:!
dense_578_167186191:
identity??!dense_576/StatefulPartitionedCall?!dense_577/StatefulPartitionedCall?!dense_578/StatefulPartitionedCallz
normalization_2/subSubnormalization_2_inputnormalization_2_sub_y*
T0*'
_output_shapes
:?????????']
normalization_2/SqrtSqrtnormalization_2_sqrt_x*
T0*
_output_shapes

:'^
normalization_2/Maximum/yConst*
_output_shapes
: *
dtype0*
valueB
 *???3?
normalization_2/MaximumMaximumnormalization_2/Sqrt:y:0"normalization_2/Maximum/y:output:0*
T0*
_output_shapes

:'?
normalization_2/truedivRealDivnormalization_2/sub:z:0normalization_2/Maximum:z:0*
T0*'
_output_shapes
:?????????'?
!dense_576/StatefulPartitionedCallStatefulPartitionedCallnormalization_2/truediv:z:0dense_576_167186179dense_576_167186181*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_576_layer_call_and_return_conditional_losses_167185965?
!dense_577/StatefulPartitionedCallStatefulPartitionedCall*dense_576/StatefulPartitionedCall:output:0dense_577_167186184dense_577_167186186*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_577_layer_call_and_return_conditional_losses_167185982?
!dense_578/StatefulPartitionedCallStatefulPartitionedCall*dense_577/StatefulPartitionedCall:output:0dense_578_167186189dense_578_167186191*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_578_layer_call_and_return_conditional_losses_167185998y
IdentityIdentity*dense_578/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp"^dense_576/StatefulPartitionedCall"^dense_577/StatefulPartitionedCall"^dense_578/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 2F
!dense_576/StatefulPartitionedCall!dense_576/StatefulPartitionedCall2F
!dense_577/StatefulPartitionedCall!dense_577/StatefulPartitionedCall2F
!dense_578/StatefulPartitionedCall!dense_578/StatefulPartitionedCall:g c
0
_output_shapes
:??????????????????
/
_user_specified_namenormalization_2_input:$ 

_output_shapes

:':$ 

_output_shapes

:'
?"
?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186297

inputs
normalization_2_sub_y
normalization_2_sqrt_x:
(dense_576_matmul_readvariableop_resource:'7
)dense_576_biasadd_readvariableop_resource::
(dense_577_matmul_readvariableop_resource:7
)dense_577_biasadd_readvariableop_resource::
(dense_578_matmul_readvariableop_resource:7
)dense_578_biasadd_readvariableop_resource:
identity?? dense_576/BiasAdd/ReadVariableOp?dense_576/MatMul/ReadVariableOp? dense_577/BiasAdd/ReadVariableOp?dense_577/MatMul/ReadVariableOp? dense_578/BiasAdd/ReadVariableOp?dense_578/MatMul/ReadVariableOpk
normalization_2/subSubinputsnormalization_2_sub_y*
T0*'
_output_shapes
:?????????']
normalization_2/SqrtSqrtnormalization_2_sqrt_x*
T0*
_output_shapes

:'^
normalization_2/Maximum/yConst*
_output_shapes
: *
dtype0*
valueB
 *???3?
normalization_2/MaximumMaximumnormalization_2/Sqrt:y:0"normalization_2/Maximum/y:output:0*
T0*
_output_shapes

:'?
normalization_2/truedivRealDivnormalization_2/sub:z:0normalization_2/Maximum:z:0*
T0*'
_output_shapes
:?????????'?
dense_576/MatMul/ReadVariableOpReadVariableOp(dense_576_matmul_readvariableop_resource*
_output_shapes

:'*
dtype0?
dense_576/MatMulMatMulnormalization_2/truediv:z:0'dense_576/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
 dense_576/BiasAdd/ReadVariableOpReadVariableOp)dense_576_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_576/BiasAddBiasAdddense_576/MatMul:product:0(dense_576/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????d
dense_576/ReluReludense_576/BiasAdd:output:0*
T0*'
_output_shapes
:??????????
dense_577/MatMul/ReadVariableOpReadVariableOp(dense_577_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
dense_577/MatMulMatMuldense_576/Relu:activations:0'dense_577/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
 dense_577/BiasAdd/ReadVariableOpReadVariableOp)dense_577_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_577/BiasAddBiasAdddense_577/MatMul:product:0(dense_577/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????d
dense_577/ReluReludense_577/BiasAdd:output:0*
T0*'
_output_shapes
:??????????
dense_578/MatMul/ReadVariableOpReadVariableOp(dense_578_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
dense_578/MatMulMatMuldense_577/Relu:activations:0'dense_578/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
 dense_578/BiasAdd/ReadVariableOpReadVariableOp)dense_578_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_578/BiasAddBiasAdddense_578/MatMul:product:0(dense_578/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????i
IdentityIdentitydense_578/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp!^dense_576/BiasAdd/ReadVariableOp ^dense_576/MatMul/ReadVariableOp!^dense_577/BiasAdd/ReadVariableOp ^dense_577/MatMul/ReadVariableOp!^dense_578/BiasAdd/ReadVariableOp ^dense_578/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 2D
 dense_576/BiasAdd/ReadVariableOp dense_576/BiasAdd/ReadVariableOp2B
dense_576/MatMul/ReadVariableOpdense_576/MatMul/ReadVariableOp2D
 dense_577/BiasAdd/ReadVariableOp dense_577/BiasAdd/ReadVariableOp2B
dense_577/MatMul/ReadVariableOpdense_577/MatMul/ReadVariableOp2D
 dense_578/BiasAdd/ReadVariableOp dense_578/BiasAdd/ReadVariableOp2B
dense_578/MatMul/ReadVariableOpdense_578/MatMul/ReadVariableOp:X T
0
_output_shapes
:??????????????????
 
_user_specified_nameinputs:$ 

_output_shapes

:':$ 

_output_shapes

:'
?
?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186103

inputs
normalization_2_sub_y
normalization_2_sqrt_x%
dense_576_167186087:'!
dense_576_167186089:%
dense_577_167186092:!
dense_577_167186094:%
dense_578_167186097:!
dense_578_167186099:
identity??!dense_576/StatefulPartitionedCall?!dense_577/StatefulPartitionedCall?!dense_578/StatefulPartitionedCallk
normalization_2/subSubinputsnormalization_2_sub_y*
T0*'
_output_shapes
:?????????']
normalization_2/SqrtSqrtnormalization_2_sqrt_x*
T0*
_output_shapes

:'^
normalization_2/Maximum/yConst*
_output_shapes
: *
dtype0*
valueB
 *???3?
normalization_2/MaximumMaximumnormalization_2/Sqrt:y:0"normalization_2/Maximum/y:output:0*
T0*
_output_shapes

:'?
normalization_2/truedivRealDivnormalization_2/sub:z:0normalization_2/Maximum:z:0*
T0*'
_output_shapes
:?????????'?
!dense_576/StatefulPartitionedCallStatefulPartitionedCallnormalization_2/truediv:z:0dense_576_167186087dense_576_167186089*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_576_layer_call_and_return_conditional_losses_167185965?
!dense_577/StatefulPartitionedCallStatefulPartitionedCall*dense_576/StatefulPartitionedCall:output:0dense_577_167186092dense_577_167186094*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_577_layer_call_and_return_conditional_losses_167185982?
!dense_578/StatefulPartitionedCallStatefulPartitionedCall*dense_577/StatefulPartitionedCall:output:0dense_578_167186097dense_578_167186099*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_dense_578_layer_call_and_return_conditional_losses_167185998y
IdentityIdentity*dense_578/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp"^dense_576/StatefulPartitionedCall"^dense_577/StatefulPartitionedCall"^dense_578/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 2F
!dense_576/StatefulPartitionedCall!dense_576/StatefulPartitionedCall2F
!dense_577/StatefulPartitionedCall!dense_577/StatefulPartitionedCall2F
!dense_578/StatefulPartitionedCall!dense_578/StatefulPartitionedCall:X T
0
_output_shapes
:??????????????????
 
_user_specified_nameinputs:$ 

_output_shapes

:':$ 

_output_shapes

:'
?

?
'__inference_signature_wrapper_167186224
normalization_2_input
unknown
	unknown_0
	unknown_1:'
	unknown_2:
	unknown_3:
	unknown_4:
	unknown_5:
	unknown_6:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallnormalization_2_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*(
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *-
f(R&
$__inference__wrapped_model_167185940o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*O
_input_shapes>
<:??????????????????:':': : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:g c
0
_output_shapes
:??????????????????
/
_user_specified_namenormalization_2_input:$ 

_output_shapes

:':$ 

_output_shapes

:'"?L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*?
serving_default?
`
normalization_2_inputG
'serving_default_normalization_2_input:0??????????????????=
	dense_5780
StatefulPartitionedCall:0?????????tensorflow/serving/predict:?\
?
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
layer_with_weights-2
layer-2
layer_with_weights-3
layer-3
	optimizer
	variables
trainable_variables
regularization_losses
		keras_api


signatures
`__call__
*a&call_and_return_all_conditional_losses
b_default_save_signature"
_tf_keras_sequential
?

_keep_axis
_reduce_axis
_reduce_axis_mask
_broadcast_shape
mean

adapt_mean
variance
adapt_variance
	count
	keras_api
c_adapt_function"
_tf_keras_layer
?

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
d__call__
*e&call_and_return_all_conditional_losses"
_tf_keras_layer
?

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
f__call__
*g&call_and_return_all_conditional_losses"
_tf_keras_layer
?

kernel
 bias
!	variables
"trainable_variables
#regularization_losses
$	keras_api
h__call__
*i&call_and_return_all_conditional_losses"
_tf_keras_layer
?
%iter

&beta_1

'beta_2
	(decay
)learning_ratemTmUmVmWmX mYvZv[v\v]v^ v_"
	optimizer
_
0
1
2
3
4
5
6
7
 8"
trackable_list_wrapper
J
0
1
2
3
4
 5"
trackable_list_wrapper
 "
trackable_list_wrapper
?
*non_trainable_variables

+layers
,metrics
-layer_regularization_losses
.layer_metrics
	variables
trainable_variables
regularization_losses
`__call__
b_default_save_signature
*a&call_and_return_all_conditional_losses
&a"call_and_return_conditional_losses"
_generic_user_object
,
jserving_default"
signature_map
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
:'2mean
:'2variance
:	 2count
"
_generic_user_object
": '2dense_576/kernel
:2dense_576/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
/non_trainable_variables

0layers
1metrics
2layer_regularization_losses
3layer_metrics
	variables
trainable_variables
regularization_losses
d__call__
*e&call_and_return_all_conditional_losses
&e"call_and_return_conditional_losses"
_generic_user_object
": 2dense_577/kernel
:2dense_577/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
4non_trainable_variables

5layers
6metrics
7layer_regularization_losses
8layer_metrics
	variables
trainable_variables
regularization_losses
f__call__
*g&call_and_return_all_conditional_losses
&g"call_and_return_conditional_losses"
_generic_user_object
": 2dense_578/kernel
:2dense_578/bias
.
0
 1"
trackable_list_wrapper
.
0
 1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
9non_trainable_variables

:layers
;metrics
<layer_regularization_losses
=layer_metrics
!	variables
"trainable_variables
#regularization_losses
h__call__
*i&call_and_return_all_conditional_losses
&i"call_and_return_conditional_losses"
_generic_user_object
:	 (2	Adam/iter
: (2Adam/beta_1
: (2Adam/beta_2
: (2
Adam/decay
: (2Adam/learning_rate
5
0
1
2"
trackable_list_wrapper
<
0
1
2
3"
trackable_list_wrapper
<
>0
?1
@2
A3"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
N
	Btotal
	Ccount
D	variables
E	keras_api"
_tf_keras_metric
N
	Ftotal
	Gcount
H	variables
I	keras_api"
_tf_keras_metric
^
	Jtotal
	Kcount
L
_fn_kwargs
M	variables
N	keras_api"
_tf_keras_metric
^
	Ototal
	Pcount
Q
_fn_kwargs
R	variables
S	keras_api"
_tf_keras_metric
:  (2total
:  (2count
.
B0
C1"
trackable_list_wrapper
-
D	variables"
_generic_user_object
:  (2total
:  (2count
.
F0
G1"
trackable_list_wrapper
-
H	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
.
J0
K1"
trackable_list_wrapper
-
M	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
.
O0
P1"
trackable_list_wrapper
-
R	variables"
_generic_user_object
':%'2Adam/dense_576/kernel/m
!:2Adam/dense_576/bias/m
':%2Adam/dense_577/kernel/m
!:2Adam/dense_577/bias/m
':%2Adam/dense_578/kernel/m
!:2Adam/dense_578/bias/m
':%'2Adam/dense_576/kernel/v
!:2Adam/dense_576/bias/v
':%2Adam/dense_577/kernel/v
!:2Adam/dense_577/bias/v
':%2Adam/dense_578/kernel/v
!:2Adam/dense_578/bias/v
?2?
2__inference_sequential_192_layer_call_fn_167186024
2__inference_sequential_192_layer_call_fn_167186245
2__inference_sequential_192_layer_call_fn_167186266
2__inference_sequential_192_layer_call_fn_167186143?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186297
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186328
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186169
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186195?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?B?
$__inference__wrapped_model_167185940normalization_2_input"?
???
FullArgSpec
args? 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
 __inference_adapt_step_167186374?
???
FullArgSpec
args?

jiterator
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
-__inference_dense_576_layer_call_fn_167186383?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
H__inference_dense_576_layer_call_and_return_conditional_losses_167186394?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
-__inference_dense_577_layer_call_fn_167186403?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
H__inference_dense_577_layer_call_and_return_conditional_losses_167186414?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
-__inference_dense_578_layer_call_fn_167186423?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
H__inference_dense_578_layer_call_and_return_conditional_losses_167186433?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
'__inference_signature_wrapper_167186224normalization_2_input"?
???
FullArgSpec
args? 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
	J
Const
J	
Const_1?
$__inference__wrapped_model_167185940?kl G?D
=?:
8?5
normalization_2_input??????????????????
? "5?2
0
	dense_578#? 
	dense_578?????????r
 __inference_adapt_step_167186374NC?@
9?6
4?1?
??????????'IteratorSpec 
? "
 ?
H__inference_dense_576_layer_call_and_return_conditional_losses_167186394\/?,
%?"
 ?
inputs?????????'
? "%?"
?
0?????????
? ?
-__inference_dense_576_layer_call_fn_167186383O/?,
%?"
 ?
inputs?????????'
? "???????????
H__inference_dense_577_layer_call_and_return_conditional_losses_167186414\/?,
%?"
 ?
inputs?????????
? "%?"
?
0?????????
? ?
-__inference_dense_577_layer_call_fn_167186403O/?,
%?"
 ?
inputs?????????
? "???????????
H__inference_dense_578_layer_call_and_return_conditional_losses_167186433\ /?,
%?"
 ?
inputs?????????
? "%?"
?
0?????????
? ?
-__inference_dense_578_layer_call_fn_167186423O /?,
%?"
 ?
inputs?????????
? "???????????
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186169?kl O?L
E?B
8?5
normalization_2_input??????????????????
p 

 
? "%?"
?
0?????????
? ?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186195?kl O?L
E?B
8?5
normalization_2_input??????????????????
p

 
? "%?"
?
0?????????
? ?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186297skl @?=
6?3
)?&
inputs??????????????????
p 

 
? "%?"
?
0?????????
? ?
M__inference_sequential_192_layer_call_and_return_conditional_losses_167186328skl @?=
6?3
)?&
inputs??????????????????
p

 
? "%?"
?
0?????????
? ?
2__inference_sequential_192_layer_call_fn_167186024ukl O?L
E?B
8?5
normalization_2_input??????????????????
p 

 
? "???????????
2__inference_sequential_192_layer_call_fn_167186143ukl O?L
E?B
8?5
normalization_2_input??????????????????
p

 
? "???????????
2__inference_sequential_192_layer_call_fn_167186245fkl @?=
6?3
)?&
inputs??????????????????
p 

 
? "???????????
2__inference_sequential_192_layer_call_fn_167186266fkl @?=
6?3
)?&
inputs??????????????????
p

 
? "???????????
'__inference_signature_wrapper_167186224?kl `?]
? 
V?S
Q
normalization_2_input8?5
normalization_2_input??????????????????"5?2
0
	dense_578#? 
	dense_578?????????