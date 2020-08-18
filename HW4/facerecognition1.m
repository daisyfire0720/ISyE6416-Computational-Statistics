addpath(genpath('C:\Users\Daisy\Desktop\yalefaces'));
clear s1; ds = 4;
tmp = imread('subject14.gif'); tmp = tmp(1:ds:240,1:ds:end);
s1(1,:) = tmp(:)';
tmp = imread('subject14.glasses.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(2,:) = tmp(:)';
tmp = imread('subject14.happy.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(3,:) = tmp(:)';
tmp = imread('subject14.leftlight.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(4,:) = tmp(:)';
tmp = imread('subject14.noglasses.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(5,:) = tmp(:)';
tmp = imread('subject14.normal.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(6,:) = tmp(:)';
tmp = imread('subject14.rightlight.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(7,:) = tmp(:)';
tmp = imread('subject14.sad.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(8,:) = tmp(:)';
tmp = imread('subject14.sleepy.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(9,:) = tmp(:)';
tmp = imread('subject14.wink.gif');tmp = tmp(1:ds:240,1:ds:end);
s1(10,:) = tmp(:)';
mean_face = mean(s1);
figure;
imagesc(reshape(mean_face,60,80)); colormap gray
title('Mean Face');

s1 = double(s1); mean_face = mean(s1);
s1_zero = (s1-repmat(mean_face,10,1));
A = s1_zero'*s1_zero;
[V, D] = eigs(A);
figure;
imagesc(reshape(V(:,1),60,80)); colormap gray
title('1st Eigenface');
figure;
imagesc(reshape(V(:,2),60,80)); colormap gray
title('2nd Eigenface');
figure;
imagesc(reshape(V(:,3),60,80)); colormap gray
title('3rd Eigenface');
figure;
imagesc(reshape(V(:,4),60,80)); colormap gray
title('4th Eigenface');
figure; 
imagesc(reshape(V(:,5),60,80)); colormap gray
title('5th Eigenface');
figure;
imagesc(reshape(V(:,6),60,80)); colormap gray
title('6th Eigenface');


