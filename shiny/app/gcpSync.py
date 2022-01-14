#-------------------------------
#        libraries
#-------------------------------

#bigquery sdk
from google.cloud import storage
#-------------------------------
#        Bucket
#-------------------------------
# Instantiates a client

def storage_client_with_creds(json_file):
    return(storage.Client.from_service_account_json(json_file))

def storage_client_without_creds():
    return(storage.Client())
  
def blob_exists(storage_client, bucket_name, filename):
   """Checks if a file exists in the bucket."""
   
   bucket = storage_client.get_bucket(bucket_name)
   blob = bucket.blob(filename)
   return (blob.exists())

def upload_blob(storage_client, bucket_name, source_file_name, destination_blob_name):
    """Uploads a file to the bucket."""
    # The ID of your GCS bucket
    # bucket_name = "your-bucket-name"
    # The path to your file to upload
    # source_file_name = "local/path/to/file"
    # The ID of your GCS object
    # destination_blob_name = "storage-object-name"
    bucket = storage_client.bucket(bucket_name)
    blob = bucket.blob(destination_blob_name)

    blob.upload_from_filename(source_file_name)

    return(
        "File {} uploaded to {} GCS directory.".format(
            source_file_name, destination_blob_name
        )
    )
    
def download_blob(storage_client, bucket_name, destination_blob_name, destination_file_name):
    """Downloads a file to the bucket."""
    # The ID of your GCS bucket
    # bucket_name = "your-bucket-name"
    # The path to your file to upload
    # source_file_name = "local/path/to/file"
    # The ID of your GCS object
    # destination_blob_name = "storage-object-name"
    bucket = storage_client.bucket(bucket_name)
    blob = bucket.blob(destination_blob_name)
    blob.download_to_filename(destination_file_name)
    
    return(
        "GCS file {} downloaded to {}.".format(
            destination_blob_name, destination_file_name
        )
    )
