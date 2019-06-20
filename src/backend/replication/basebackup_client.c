#include "postgres.h"

#include <unistd.h>

#include "replication/basebackup_client.h"
#include "replication/walreceiver.h"
#include "utils/guc.h"

void
BaseBackupMain(void)
{
	WalReceiverConn *wrconn = NULL;
	char	   *err;
	//WalRcvExecResult *res;

	elog(LOG, "BaseBackupMain()");

	/* Load the libpq-specific functions */
	load_file("libpqwalreceiver", false);
	if (WalReceiverFunctions == NULL)
		elog(ERROR, "libpqwalreceiver didn't initialize correctly");

	/* Establish the connection to the primary */
	wrconn = walrcv_connect(PrimaryConnInfo, false, cluster_name[0] ? cluster_name : "basebackup", &err);
	if (!wrconn)
		ereport(ERROR,
				(errmsg("could not connect to the primary server: %s", err)));

	sleep(10);
	//res = walrcv_exec("BASE_BACKUP")

	walrcv_disconnect(wrconn);
}
