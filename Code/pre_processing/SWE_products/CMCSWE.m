function CMC = CMCSWE(FileName)
% Syntax
%
%       CMC = CMCSWE(...)
% 
% Discription
%
% The purpose of this function is to read the monthly CMC_SWE space 
% deliminiated data from 1998 until current release and seperate different montha 
% then construc them in one matrix. 
%
% [SWE,Month,GeoCoord] = CMCSWE(FILENAME,PATHNAME) read the FileName and PathName 
% which are obtained from uigetfile (). Then reading the source data using 
% textread(). 
% The outputs of CMCSWE() include SWE,Month,GeoCoord respectively. The SWE 
% matrix contain stored CMC_SWE products which are stored in multidemsional
% matrix. The MONTH matrix contains information related to CMC_SWE stored
% month. The GEOCOORD matrix includes information related to longitudinal
% and latitudinal stored CMC_SWE products
%
% Input 
%
%       prmname                 Monthly CMC text files and  geographical
%                               inptut files
%
%
% Output      
% 
%                               saved CMC SWE matrix                  
%
% Reference 
%
% Created:          15/04/2015
%
% last modified     12/14/2016
%
% Author:           Ala Bahrami
%
% Note : The recent release of CMC SWE products should be checked.  
%
% Todo : uigetfile should be replaced with reading input files from a
% parameter file. 
%
%% Copyright (C) 2021 Ala Bahrami 
%% Reading and constructing the data 
% Don't foreget to select files correctly 
    if nargin==0 
       [FileName,~] = uigetfile('*.txt','Select the CMC SWE and Geographic Coordinate Files','Multiselect','on');
    end 
    SW = textread(FileName{1});
    N = size(SW,1);   
    M = N/707;                    % Define number of months 
    SWE = zeros(707,706,M);
    Month = zeros(M,2);
    % Constructing SWE matrix 
    for i = 1:M
        SWE(:,:,i) = SW((i-1)*707+1:(i*707),:);
        Month(i,:) = SW((i-1)*707+1,1:2);
    end
    %Subsitude the unclassifed snow and water mask with NaN values because of the fact
    %that the values of baseline for these pixels will be -999, so the value 
    % anomalies will be zero. 
    SWE(SWE==-999) = NaN;

    SWE(1,:,:) = [];
    GeoCoord = textread(FileName{2});
    CMC.SWE = SWE;
    CMC.GeoCoord = GeoCoord;
    CMC.Month = Month;

%% Constructing the longitude and Latitude matrices 
    Lat = (reshape(GeoCoord(:,3),706,706))';
    Long = (reshape(GeoCoord(:,4),706,706))';
    CMC.Lat = Lat;
    CMC.Long = Long;

%% Finding rows of second baseline 
    [r1 , ~] = find(Month(:,1)==2002 & Month(:,2)==6);
    [r2 , ~] = find(Month(:,1)==2011 & Month(:,2)==6);
    [r3 , ~] = find(Month(:,1)==2002 & Month(:,2)==4);

%% Computing CMC SWE Baseline from 1998-2014
    for i=1:706
        for j=1:706
            CMC.SWEBaseline(i,j) = mean(CMC.SWE(i,j,:));
        end
    end 

%% Computing CMC SWE Baseline from 06/2002-09/2011
    % It should be noted that the CMC_SWE data on 09/2011 isn't avalaile
    for i=1:706
        for j=1:706
            CMC.SWEBaseline2(i,j) = mean(CMC.SWE(i,j,r1:r2));
        end
    end

%%  Computing CMC SWE Baseline from 04/2002-12/2014
    for i=1:706
        for j=1:706
            CMC.SWEBaseline3(i,j) = mean(CMC.SWE(i,j,r3:M));
        end
    end
    
%% Creating CMC SWE anomalies based on the Baseline 1998-2014
    for i=1:M
        CMC.SWEAnom(:,:,i)=CMC.SWE(:,:,i)-CMC.SWEBaseline;
    end 

%% Creating CMC SWE anomalies based on the Baseline 06/2002-09/2011
    for i=r1:r2
        CMC.SWEAnom2(:,:,i-r1+1) = CMC.SWE(:,:,i)-CMC.SWEBaseline2;
    end 

%% Creating CMC SWE anomalies based on the Baseline 04/2002-12/2014
    for i=r3:M
        CMC.SWEAnom3(:,:,i-r3+1) = CMC.SWE(:,:,i)-CMC.SWEBaseline3;
    end

    % Saving CMCSWE data 
    save('CMCSWE.mat','CMC')

end 